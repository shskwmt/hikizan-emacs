import os
from pathlib import Path


def create_plan_file() -> str:
    """
    Creates an empty plan Org-Mode file for the current session and returns its path.
    The path is relative to the agent's root directory.
    """
    session_id = os.environ.get("SESSION_ID", "default_session")
    # We want the plan directory to be ~/.emacs.d/python/emacs_agent/plans/
    # The agent's root directory is usually where main.py is, or its parent.
    # From tools/plans.py, the agent root (emacs_agent/) is one level up.

    # Let's find the absolute path of ~/.emacs.d/python/emacs_agent/plans/
    base_dir = Path(__file__).resolve().parent.parent
    plans_dir = base_dir / "plans"
    plans_dir.mkdir(parents=True, exist_ok=True)

    plan_file_name = f"{session_id}_task_list.org"
    plan_file_path = plans_dir / plan_file_name

    if not plan_file_path.exists():
        plan_file_path.touch()

    # Return the relative path from ~/.emacs.d/
    # But wait, it might be simpler to return the absolute path or something consistent.
    # Most agents use absolute paths via expand-file-name in elisp.
    # Let's return the absolute path for clarity.
    return str(plan_file_path).replace("\\", "/")
