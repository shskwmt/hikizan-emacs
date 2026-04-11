import os
import re
from pathlib import Path


def create_plan_file(new_task: bool = False) -> str:
    """
    Creates or returns a plan Org-Mode file for the current session.
    If new_task is True, it creates a new versioned file if the base file exists.
    Returns the absolute path.
    """
    session_id = os.environ.get("SESSION_ID", "default_session")
    base_dir = Path(__file__).resolve().parent.parent
    plans_dir = base_dir / "plans"
    plans_dir.mkdir(parents=True, exist_ok=True)

    base_name = f"{session_id}_task_list"
    
    # Find existing versions
    existing_files = list(plans_dir.glob(f"{base_name}*.org"))
    
    # Helper to extract version number
    def get_version(path):
        match = re.search(r"_v(\d+)\.org$", path.name)
        return int(match.group(1)) if match else 0

    existing_files.sort(key=get_version)
    
    if not existing_files:
        # Create initial base file
        plan_file_path = plans_dir / f"{base_name}.org"
        plan_file_path.touch()
    elif new_task:
        # Create new version
        last_version = get_version(existing_files[-1])
        new_version = last_version + 1
        plan_file_path = plans_dir / f"{base_name}_v{new_version}.org"
        plan_file_path.touch()
    else:
        # Use latest version
        plan_file_path = existing_files[-1]

    return str(plan_file_path).replace("\\", "/")
