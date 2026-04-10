import os
import shutil
import subprocess
import time


def get_emacsclient_bin() -> str:
    """Get the emacsclient binary name based on the OS."""
    return "emacsclientw" if os.name == "nt" else "emacsclient"


def get_emacsclient_flag() -> str:
    """Get the emacsclient flag for specifying the server/socket."""
    return "-f" if os.name == "nt" else "-s"


def start_emacs_daemon(session_id: str, server_file: str):
    """Start a new Emacs daemon with session_id as the server name."""
    print(f"Starting dedicated Emacs daemon: {session_id}")

    # Try to find emacs executable
    emacs_bin = shutil.which("emacs")
    if not emacs_bin:
        # Fallback for Windows if not in PATH
        if os.name == "nt":
            # You might want to provide a better way to find it or use a default path
            emacs_bin = "emacs"
        else:
            emacs_bin = "emacs"

    # On Windows, we might need to handle it differently if we want it to stay alive
    # start-process in Elisp uses --fg-daemon.
    command = [emacs_bin, f"--fg-daemon={session_id}"]

    # We use Popen so it runs in background
    # Note: --fg-daemon means it stays in foreground from the process's perspective,
    # but we run it as a subprocess.
    proc = subprocess.Popen(
        command,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        encoding="utf-8",
        errors="replace",
        text=True,
        # On Windows, creationflags=subprocess.CREATE_NEW_PROCESS_GROUP might be useful
        # but for daemon it usually detaches anyway.
    )

    # Wait until daemon is ready (server file exists)
    # Actually, server-running-p in Elisp is more reliable.
    # But from Python, we can check for the server file.

    timeout = 120
    while timeout > 0:
        if os.path.exists(server_file):
            print(f"Emacs daemon {session_id} is ready.")
            return proc
        if proc.poll() is not None:
            raise RuntimeError(
                f"Emacs daemon failed to start with exit code {proc.returncode}."
            )
        time.sleep(0.5)
        timeout -= 0.5

    raise RuntimeError(f"Timeout waiting for Emacs daemon {session_id} to start.")


def start_emacs_client(session_id: str):
    """Start emacsclient for the given session_id."""
    client_bin = get_emacsclient_bin()
    flag = get_emacsclient_flag()

    # Using -t for terminal mode as in the original Elisp code
    command = [client_bin, flag, session_id, "-t"]

    print(f"Launching emacsclient: {' '.join(command)}")

    # We use Popen with creationflags=subprocess.CREATE_NEW_CONSOLE on Windows
    # if we want a new window for the client.
    kwargs = {}
    if os.name == "nt":
        kwargs["creationflags"] = subprocess.CREATE_NEW_CONSOLE

    subprocess.Popen(command, **kwargs)


def kill_emacs_daemon(session_id: str):
    """Kill the Emacs daemon with session_id."""
    client_bin = get_emacsclient_bin()
    flag = get_emacsclient_flag()
    command = [client_bin, flag, session_id, "-e", "(kill-emacs)"]
    print(f"Killing Emacs daemon: {session_id}")
    try:
        subprocess.run(command, capture_output=True, encoding="utf-8", errors="replace")
    except Exception as e:
        print(f"Error killing daemon: {e}")
