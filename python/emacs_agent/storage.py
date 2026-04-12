import os
from pathlib import Path

# Base directory for all sessions. Can be overridden by EMACS_AGENT_SESSIONS_BASE_DIR env var.
EMACS_AGENT_SESSIONS_BASE_DIR = Path(
    os.environ.get(
        "EMACS_AGENT_SESSIONS_BASE_DIR", 
        Path(__file__).resolve().parent / "sessions"
    )
)


def get_session_dir(session_id: str) -> Path:
    """
    Returns the directory for a given session, creating it if necessary.
    """
    session_dir = EMACS_AGENT_SESSIONS_BASE_DIR / session_id
    session_dir.mkdir(parents=True, exist_ok=True)
    return session_dir
