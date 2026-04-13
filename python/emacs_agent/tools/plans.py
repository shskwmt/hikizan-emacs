import os

from ..storage import create_versioned_file


def create_plan_file(new_task: bool = False) -> str:
    """
    Creates or returns a plan Org-Mode file for the current session.
    If new_task is True, it creates a new versioned file if the base file exists.
    Returns the absolute path.
    """
    session_id = os.environ.get("SESSION_ID", "default_session")
    return create_versioned_file(session_id, "plan", "org", new_task)
