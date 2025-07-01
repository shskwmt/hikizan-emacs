import fnmatch
import os
import subprocess
import tempfile
import time

from pydantic import BaseModel, Field
from langchain_core.tools import tool

# --- Tools ---

# Tool for executing Emacs Lisp
def write_elisp_code_to_temp_file(code: str) -> str:
    """
    Create a temp file and write the provided code into the temp file.
    Returns the temp file path.
    """
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.el') as temp_file:
        temp_file.write(code)
        file_path = temp_file.name.replace("\\", "/")
        return file_path

def eval_elisp_code(code: str) -> str:
    """
    Executes the Emacs Lisp code.
    Returns the result or an error message.
    """
    temp_file_path = write_elisp_code_to_temp_file(code)
    print(f"Emacs LISP code written to: {temp_file_path}")

    command = f"emacsclientw.exe -e \"(hikizan/eval-elisp-file \\\"{temp_file_path}\\\")\""

    try:
        subprocess.run(command, shell=True, check=True, text=True, capture_output=True)
        time.sleep(1)
        temp_log_file_path = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.log').name.replace("\\", "/")
        log_command = f"emacsclientw.exe -e \"(hikizan/write-string-to-file \\\"{temp_log_file_path}\\\" (hikizan/get-string-from-point (get-buffer \\\"*Messages*\\\") (hikizan/find-string-position-in-buffer (get-buffer \\\"*Messages*\\\") \\\"{temp_file_path}\\\")))\""

        subprocess.run(log_command, shell=True, check=True, text=True, capture_output=True)
        print(f"Log written to: {temp_log_file_path}")

        with open(temp_log_file_path, 'r', encoding='utf-8') as log_file:
            content = log_file.read()

        return content
    except Exception as e:
        return f"Error: {str(e)}"
class ElispCode(BaseModel):
    code: str = Field(description="The Emacs Lisp code to execute. It must print its result to be captured.")

@tool(args_schema=ElispCode)
def execute_elisp_code(code: str) -> str:
    """
    Executes the Emacs Lisp code.
    Returns the result or an error message.
    """
    return eval_elisp_code(code)

class ReadBufferArgs(BaseModel):
    buffer_name: str = Field(description="The emacs buffer name to read.")

@tool(args_schema=ReadBufferArgs)
def read_buffer(buffer_name: str) -> str:
    """Reads the contents of a buffer in emacs and returns them as a string."""
    return eval_elisp_code(f'(with-current-buffer "{buffer_name}" (message "%s" (buffer-string)))')

# File System Tools
class ReadFileArgs(BaseModel):
    file_path: str = Field(description="The path to the file to read.")

@tool(args_schema=ReadFileArgs)
def read_file(file_path: str) -> str:
    """Reads the contents of a file and returns them as a string."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return f.read()
    except FileNotFoundError:
        return f"Error: File not found at {file_path}"
    except Exception as e:
        return f"An unexpected error occurred while reading the file: {str(e)}"

class WriteToFileArgs(BaseModel):
    file_path: str = Field(description="The path to the file to write to.")
    content: str = Field(description="The content to write to the file.")

@tool(args_schema=WriteToFileArgs)
def write_to_file(file_path: str, content: str) -> str:
    """Writes the given content to a specified file, overwriting it if it exists."""
    try:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        return f"Successfully wrote to {file_path}"
    except Exception as e:
        return f"An unexpected error occurred while writing to the file: {str(e)}"

class ListFilesArgs(BaseModel):
    path: str = Field(description="The directory path to list files from. Defaults to the current directory.", default=".")

@tool(args_schema=ListFilesArgs)
def list_files(path: str = ".") -> str:
    """Lists all files and directories in a given path."""
    try:
        if not os.path.isdir(path):
            return f"Error: The path '{path}' is not a valid directory."
        files = os.listdir(path)
        if not files:
            return f"The directory '{path}' is empty."
        return "\n".join(files)
    except Exception as e:
        return f"An unexpected error occurred while listing files: {str(e)}"

# Grep and Find Tools
class GrepArgs(BaseModel):
    pattern: str = Field(description="The regex pattern to search for.")
    path: str = Field(description="The directory or file to search in. Defaults to the current directory.", default=".")
    ignore_case: bool = Field(description="If True, performs a case-insensitive search.", default=False)

@tool(args_schema=GrepArgs)
def grep(pattern: str, path: str = ".", ignore_case: bool = False) -> str:
    """
    Searches for a pattern in files recursively using ripgrep.
    Returns matching lines with file paths and line numbers.
    """
    if not os.path.exists(path):
        return f"Error: Path '{path}' does not exist."

    try:
        command = ['rg', '--no-heading', '--with-filename', '--line-number']
        if ignore_case:
            command.append('--ignore-case')

        command.extend([pattern, path])

        result = subprocess.run(command, capture_output=True, text=True, check=True, encoding='utf-8')

        output = result.stdout.strip()
        if not output:
            return f"No matches found for pattern '{pattern}' in '{path}'."

        return output
    except FileNotFoundError:
        return "Error: 'rg' (ripgrep) command not found. Please ensure ripgrep is installed and in your PATH."
    except subprocess.CalledProcessError as e:
        # ripgrep exits with 1 if no matches are found.
        if e.returncode == 1 and not e.stdout and not e.stderr:
            return f"No matches found for pattern '{pattern}' in '{path}'."
        return f"Error executing ripgrep: {e.stderr}"
    except Exception as e:
        return f"An unexpected error occurred while running ripgrep: {str(e)}"

class FindFilesArgs(BaseModel):
    name_pattern: str = Field(description="The glob pattern for the file or directory name (e.g., '*.py', 'my_dir').")
    path: str = Field(description="The directory to start the search from. Defaults to the current directory.", default=".")
    file_type: str = Field(description="Type of item to find: 'f' for file, 'd' for directory. Defaults to finding both.", default=None)

@tool(args_schema=FindFilesArgs)
def find_files(name_pattern: str, path: str = ".", file_type: str = None) -> str:
    """
    Finds files or directories by name pattern recursively.
    """
    if not os.path.isdir(path):
        return f"Error: The path '{path}' is not a valid directory."

    matches = []
    for root, dirs, files in os.walk(path):
        if file_type != 'f':
            for d in fnmatch.filter(dirs, name_pattern):
                matches.append(os.path.join(root, d).replace('\\\\', '/'))
        if file_type != 'd':
            for f in fnmatch.filter(files, name_pattern):
                matches.append(os.path.join(root, f).replace('\\\\', '/'))

    if not matches:
        return f"No items found matching pattern '{name_pattern}' in '{path}'."

    return "\n".join(matches)
