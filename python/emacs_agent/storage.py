import os
import re
from pathlib import Path

# Base directory for all sessions. Can be overridden by EMACS_AGENT_SESSIONS_BASE_DIR env var.
EMACS_AGENT_SESSIONS_BASE_DIR = Path(
    os.environ.get(
        "EMACS_AGENT_SESSIONS_BASE_DIR", Path(__file__).resolve().parent / "sessions"
    )
)


def get_session_dir(session_id: str) -> Path:
    """
    Returns the directory for a given session, creating it if necessary.
    """
    session_dir = EMACS_AGENT_SESSIONS_BASE_DIR / session_id
    session_dir.mkdir(parents=True, exist_ok=True)
    return session_dir


def create_versioned_file(
    session_id: str, base_name: str, ext: str, new_file: bool = False
) -> str:
    """
    Creates or returns a versioned file for the current session.
    If new_file is True, it creates a new versioned file if the base file exists.
    Returns the absolute path as a string.
    """
    session_dir = get_session_dir(session_id)

    # Find existing versions
    existing_files = list(session_dir.glob(f"{base_name}*.{ext}"))

    # Helper to extract version number
    def get_version(path):
        match = re.search(rf"_v(\d+)\.{ext}$", path.name)
        return int(match.group(1)) if match else 0

    existing_files.sort(key=get_version)

    if not existing_files:
        # Create initial base file
        file_path = session_dir / f"{base_name}.{ext}"
        file_path.touch()
    elif new_file:
        # Create new version
        last_version = get_version(existing_files[-1])
        new_version = last_version + 1
        file_path = session_dir / f"{base_name}_v{new_version}.{ext}"
        file_path.touch()
    else:
        # Use latest version
        file_path = existing_files[-1]

    return str(file_path).replace("\\", "/")
