import os

from ..storage import create_versioned_file


def create_issue_file(new_issue: bool = False) -> str:
    """
    Creates or returns an issue Org-Mode file for the current session.
    If new_issue is True, it creates a new versioned file if the base file exists.
    Returns the absolute path.
    """
    session_id = os.environ.get("SESSION_ID", "default_session")
    return create_versioned_file(session_id, "issue", "org", new_issue)
