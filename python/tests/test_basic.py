import subprocess
import sys
from pathlib import Path


def test_ruff_lint():
    """Ensure ruff lint passes on the project."""
    project_root = Path(__file__).parent.parent
    result = subprocess.run(
        [sys.executable, "-m", "ruff", "check", "."],
        cwd=project_root,
        capture_output=True,
        encoding="utf-8", errors="replace",
    )
    assert result.returncode == 0, f"Ruff lint failed:\n{result.stdout}"


def test_ruff_format():
    """Ensure ruff format check passes on the project."""
    project_root = Path(__file__).parent.parent
    result = subprocess.run(
        [sys.executable, "-m", "ruff", "format", "--check", "."],
        cwd=project_root,
        capture_output=True,
        encoding="utf-8", errors="replace",
    )
    assert result.returncode == 0, f"Ruff format check failed:\n{result.stdout}"
