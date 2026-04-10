import os
import subprocess
import tempfile
import time

MAX_OUTPUT_LENGTH = 3000


def get_target_directory() -> str | None:
    target_dir = os.environ.get("ELISP_TEMP_PATH")
    if not target_dir:
        return None
    try:
        os.makedirs(target_dir, exist_ok=True)
        return target_dir
    except OSError as e:
        print(
            f"Warning: Failed to create '{target_dir}'. Reverting to system default. Error: {e}"
        )
        return None


def write_elisp_code_to_temp_file(code: str) -> str:
    target_dir = get_target_directory()
    # Using delete=False because Emacs needs to read it after the Python 'with' block closes
    with tempfile.NamedTemporaryFile(
        mode="w",
        delete=False,
        suffix=".el",
        dir=target_dir,
        encoding="utf-8",
        errors="replace",
    ) as temp_file:
        temp_file.write(code)
        return temp_file.name.replace("\\", "/")


def get_emacsclient_args() -> list[str]:
    """Returns the base emacsclient command as a list of arguments."""
    args = ["emacsclient"]
    server_file = os.environ.get("EMACS_SERVER_FILE")
    if server_file:
        args.extend(["-f", server_file])
    return args


def execute_elisp_code(code: str) -> str:
    temp_file_path = None
    temp_log_path = None
    try:
        temp_file_path = write_elisp_code_to_temp_file(code)
        base_args = get_emacsclient_args()

        exec_elisp = f'(hikizan-eval-elisp-file "{temp_file_path}")'

        # Added errors="replace" here to prevent the reader thread from crashing
        subprocess.run(
            base_args + ["-e", exec_elisp],
            check=True,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
        )

        time.sleep(0.3)

        target_dir = get_target_directory()
        with tempfile.NamedTemporaryFile(
            mode="w", delete=False, suffix=".log", dir=target_dir
        ) as log_tmp:
            temp_log_path = log_tmp.name.replace("\\", "/")

        log_elisp = (
            f'(hikizan-write-string-to-file "{temp_log_path}" '
            f'(hikizan-get-string-from-point (get-buffer "*Messages*") '
            f'(hikizan-find-string-position-in-buffer (get-buffer "*Messages*") "{temp_file_path}")))'
        )

        # Added errors="replace" here as well
        subprocess.run(
            base_args + ["-e", log_elisp],
            check=True,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
        )

        with open(temp_log_path, encoding="utf-8", errors="replace") as log_file:
            output = log_file.read().strip()

        return output

    except subprocess.CalledProcessError as e:
        # e.stderr will now contain "" instead of crashing the thread
        return f"Error: Emacsclient failed (Exit {e.returncode}). Stderr: {e.stderr}"
    except Exception as e:
        return f"Error: {str(e)}"
    finally:
        if temp_file_path and os.path.exists(temp_file_path):
            os.remove(temp_file_path)
        if temp_log_path and os.path.exists(temp_log_path):
            os.remove(temp_log_path)
