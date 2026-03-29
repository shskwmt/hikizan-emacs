import os
import subprocess
import tempfile
import time
from typing import Optional

def get_target_directory() -> Optional[str]:
    """
    Determines the target directory based on 'ELISP_TEMP_PATH'.
    Returns the path string if set and valid, or None to use system default.
    """
    target_dir = os.environ.get('ELISP_TEMP_PATH')

    # If the variable is not set or is an empty string, return None immediately
    if not target_dir:
        return None

    try:
        # Try to create the directory
        os.makedirs(target_dir, exist_ok=True)
        return target_dir
    except OSError as e:
        # If creation fails (e.g., permissions), log it and fall back to None
        print(f"Warning: Failed to create '{target_dir}'. Reverting to system default. Error: {e}")
        return None

    
def write_elisp_code_to_temp_file(code: str) -> str:
    """Create a temp file and write the provided code into it."""

    # Get the directory from our helper function
    target_dir = get_target_directory()

    # Pass the result (path or None) to tempfile
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.el', dir=target_dir, encoding='utf-8') as temp_file:
        temp_file.write(code)
        return temp_file.name.replace("\\", "/")

def execute_elisp_code(code: str) -> str:
    """
    Executes Emacs Lisp code and returns the result or an error message.
    The code must print its result to be captured (e.g., using the `message` function).
    """
    temp_file_path = write_elisp_code_to_temp_file(code)
    command = f"emacsclient -e \"(hikizan/eval-elisp-file \\\"{temp_file_path}\\\")\""
    try:
        subprocess.run(command, shell=True, check=True, encoding='utf-8', capture_output=True)
        time.sleep(0.5)
        target_dir = get_target_directory()
        temp_log_file_path = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.log', dir=target_dir, encoding='utf-8').name.replace("\\", "/")
        log_command = f"emacsclient -e \"(hikizan/write-string-to-file \\\"{temp_log_file_path}\\\" (hikizan/get-string-from-point (get-buffer \\\"*Messages*\\\") (hikizan/find-string-position-in-buffer (get-buffer \\\"*Messages*\\\") \\\"{temp_file_path}\\\")))\""
        subprocess.run(log_command, shell=True, check=True, encoding='utf-8', capture_output=True)
        with open(temp_log_file_path, 'r', encoding='utf-8') as log_file:
            return log_file.read().strip()
    except Exception as e:
        return f"Error: {str(e)}"
