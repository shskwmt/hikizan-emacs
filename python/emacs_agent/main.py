import asyncio
import sys
from datetime import datetime
from pathlib import Path

import click
from google.adk.apps.app import App
from google.adk.auth.credential_service.in_memory_credential_service import (
    InMemoryCredentialService,
)
from google.adk.cli.service_registry import load_services_module
from google.adk.cli.utils import envs, logs
from google.adk.cli.utils.agent_loader import AgentLoader
from google.adk.cli.utils.service_factory import (
    create_artifact_service_from_options,
    create_memory_service_from_options,
    create_session_service_from_options,
)
from google.adk.runners import Runner
from google.adk.sessions.session import Session
from google.adk.utils.context_utils import Aclosing
from google.genai import types
from pydantic import BaseModel


class InputFile(BaseModel):
    state: dict[str, object]
    queries: list[str]


async def run_input_file(
    app_name: str,
    user_id: str,
    agent_or_app,
    artifact_service,
    session_service,
    credential_service,
    input_path: str,
    memory_service=None,
) -> Session:
    app = (
        agent_or_app
        if isinstance(agent_or_app, App)
        else App(name=app_name, root_agent=agent_or_app)
    )
    runner = Runner(
        app=app,
        artifact_service=artifact_service,
        session_service=session_service,
        memory_service=memory_service,
        credential_service=credential_service,
    )
    try:
        with open(input_path, encoding="utf-8") as f:
            input_file = InputFile.model_validate_json(f.read())
        input_file.state["_time"] = datetime.now().isoformat()

        session = await session_service.create_session(
            app_name=app_name, user_id=user_id, state=input_file.state
        )
        for query in input_file.queries:
            click.echo(f"[user]: {query}")
            content = types.Content(role="user", parts=[types.Part(text=query)])
            async with Aclosing(
                runner.run_async(
                    user_id=session.user_id, session_id=session.id, new_message=content
                )
            ) as agen:
                async for event in agen:
                    if event.content and event.content.parts:
                        if text := "".join(
                            part.text or "" for part in event.content.parts
                        ):
                            click.echo(f"[{event.author}]: {text}")
        return session
    finally:
        await runner.close()


async def run_interactively(
    root_agent_or_app,
    artifact_service,
    session,
    session_service,
    credential_service,
    memory_service=None,
) -> None:
    app = (
        root_agent_or_app
        if isinstance(root_agent_or_app, App)
        else App(name=session.app_name, root_agent=root_agent_or_app)
    )
    runner = Runner(
        app=app,
        artifact_service=artifact_service,
        session_service=session_service,
        memory_service=memory_service,
        credential_service=credential_service,
    )
    try:
        while True:
            try:
                query = input("[user]: ")
            except EOFError:
                break
            if not query or not query.strip():
                continue
            if query == "exit":
                break
            async with Aclosing(
                runner.run_async(
                    user_id=session.user_id,
                    session_id=session.id,
                    new_message=types.Content(
                        role="user", parts=[types.Part(text=query)]
                    ),
                )
            ) as agen:
                async for event in agen:
                    if event.content and event.content.parts:
                        if text := "".join(
                            part.text or "" for part in event.content.parts
                        ):
                            click.echo(f"[{event.author}]: {text}")
    finally:
        await runner.close()


async def run_main(
    agent_path: str,
    save_session: bool,
    session_id: str | None = None,
    session_service_uri: str | None = None,
    artifact_service_uri: str | None = None,
    memory_service_uri: str | None = None,
    use_local_storage: bool = True,
    replay: str | None = None,
    resume: str | None = None,
) -> None:
    logs.log_to_tmp_folder()

    agent_path_obj = Path(agent_path).resolve()
    agent_parent_folder = str(agent_path_obj.parent)
    agent_folder_name = agent_path_obj.name

    if agent_parent_folder not in sys.path:
        sys.path.insert(0, agent_parent_folder)

    load_services_module(str(agent_path_obj))
    user_id = "test_user"

    agent_loader = AgentLoader(agents_dir=agent_parent_folder)
    agent_or_app = agent_loader.load_agent(agent_folder_name)

    session_app_name = (
        agent_or_app.name if isinstance(agent_or_app, App) else agent_folder_name
    )
    app_name_to_dir = None
    if isinstance(agent_or_app, App) and agent_or_app.name != agent_folder_name:
        app_name_to_dir = {agent_or_app.name: agent_folder_name}

    envs.load_dotenv_for_agent(agent_folder_name, agent_parent_folder)

    session_service = create_session_service_from_options(
        base_dir=Path(agent_parent_folder),
        session_service_uri=session_service_uri,
        app_name_to_dir=app_name_to_dir,
        use_local_storage=use_local_storage,
    )

    artifact_service = create_artifact_service_from_options(
        base_dir=agent_path_obj,
        artifact_service_uri=artifact_service_uri,
        use_local_storage=use_local_storage,
    )
    memory_service = create_memory_service_from_options(
        base_dir=Path(agent_parent_folder),
        memory_service_uri=memory_service_uri,
    )

    credential_service = InMemoryCredentialService()

    def _print_event(event) -> None:
        content = event.content
        if not content or not content.parts:
            return
        text_parts = [part.text for part in content.parts if part.text]
        if not text_parts:
            return
        author = event.author or "system"
        click.echo(f"[{author}]: {''.join(text_parts)}")

    session = None
    try:
        if replay:
            session = await run_input_file(
                app_name=session_app_name,
                user_id=user_id,
                agent_or_app=agent_or_app,
                artifact_service=artifact_service,
                session_service=session_service,
                credential_service=credential_service,
                input_path=replay,
                memory_service=memory_service,
            )
        elif resume:
            with open(resume, encoding="utf-8") as f:
                loaded_session = Session.model_validate_json(f.read())

            session = await session_service.create_session(
                app_name=session_app_name,
                user_id=user_id,
                state=loaded_session.state if loaded_session else None,
            )

            if loaded_session:
                for event in loaded_session.events:
                    await session_service.append_event(session, event)
                    _print_event(event)

            await run_interactively(
                agent_or_app,
                artifact_service,
                session,
                session_service,
                credential_service,
                memory_service=memory_service,
            )
        else:
            session = await session_service.create_session(
                app_name=session_app_name, user_id=user_id
            )
            click.echo(f"Running agent {agent_or_app.name}, type exit to exit.")
            await run_interactively(
                agent_or_app,
                artifact_service,
                session,
                session_service,
                credential_service,
                memory_service=memory_service,
            )
    finally:
        if save_session and session:
            if not session_id:
                try:
                    session_id = input("Session ID to save: ")
                except EOFError:
                    session_id = "default_session"

            if session_id:
                session_path = agent_path_obj / f"{session_id}.session.json"

                # Fetch the session again to get all the details.
                session = await session_service.get_session(
                    app_name=session.app_name,
                    user_id=session.user_id,
                    session_id=session.id,
                )
                session_path.write_text(
                    session.model_dump_json(indent=2, exclude_none=True, by_alias=True),
                    encoding="utf-8",
                )

                print("Session saved to", session_path)


@click.command(
    context_settings=dict(ignore_unknown_options=True, allow_extra_args=True)
)
@click.option("--save_session", is_flag=True, default=False)
@click.option("--session_id", type=str)
@click.option("--session_service_uri", type=str)
@click.option("--artifact_service_uri", type=str)
@click.option("--memory_service_uri", type=str)
@click.option("--use_local_storage/--no_use_local_storage", default=True)
@click.option("--replay", type=click.Path(exists=True))
@click.option("--resume", type=click.Path(exists=True))
@click.argument("agent", type=click.Path(exists=True))
def main(
    agent: str,
    save_session: bool,
    session_id: str | None,
    session_service_uri: str | None,
    artifact_service_uri: str | None,
    memory_service_uri: str | None,
    use_local_storage: bool,
    replay: str | None,
    resume: str | None,
):
    asyncio.run(
        run_main(
            agent_path=agent,
            save_session=save_session,
            session_id=session_id,
            session_service_uri=session_service_uri,
            artifact_service_uri=artifact_service_uri,
            memory_service_uri=memory_service_uri,
            use_local_storage=use_local_storage,
            replay=replay,
            resume=resume,
        )
    )


if __name__ == "__main__":
    main()
