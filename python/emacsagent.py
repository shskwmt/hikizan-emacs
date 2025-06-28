# import packages
import os
import subprocess
import tempfile
import time
import re
import fnmatch

from typing import Annotated, Sequence, TypedDict
from pydantic import BaseModel, Field

from langchain_core.messages import BaseMessage, ToolMessage, SystemMessage, HumanMessage
from langchain_core.runnables import RunnableConfig
from langchain_core.tools import tool
from langchain_google_genai import ChatGoogleGenerativeAI
from langgraph.graph import END, StateGraph
from langgraph.graph.message import add_messages

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
MODEL_NAME = "gemini-2.5-pro"
LLM_TEMPERATURE = 0
THINKING_BUDGET = 128
TIMEOUT = 60
SYSTEM_PROMPT = """
** Role
You are a large language model living in Emacs, a powerful coding assistant.

** Context
- I am using emacs to automate programming tasks.
- You have access to the local file system to read, write, list, find, and search files.

** Instructions
1.  **Think**: Analyze the user's request and create a clear, step-by-step plan.
2.  **Explore (If Necessary)**: If you need to understand the project structure, use `list_files`, `find_files`, or `grep` to see what's in the current directory or project.
3.  **Read (If Necessary)**: Before making changes to an existing file, ALWAYS use `read_file` to get its current content. This is critical to ensure you don't overwrite important information.
4.  **Propose Changes**: Based on the file's content and the user's request, formulate the new code or modifications.
5.  **Act**:
    *   To create a new file or modify an existing one, use `write_to_file`.
    *   For tasks specific to the Emacs environment (like managing buffers), use `execute_elisp_code`.
6.  **Respond**: Inform the user about the actions you have taken.

** Tool Reference
- `list_files(path: str) -> str`: Lists files and directories in a given path.
- `read_file(file_path: str) -> str`: Reads the content of a file.
- `write_to_file(file_path: str, content: str) -> str`: Writes content to a file.
- `find_files(name_pattern: str, path: str = ".", file_type: str = None) -> str`: Finds files or directories by a glob pattern (e.g., '*.py'). `file_type` can be 'f' for files or 'd' for directories.
- `grep(pattern: str, path: str = ".", ignore_case: bool = False) -> str`: Searches for a regex pattern in files recursively and returns matching lines.
- `execute_elisp_code(code: str) -> str`: Executes Emacs Lisp code. Must print the result to be captured.

** Instructions of the `execute_elisp_code`

- You must print the result if you want to get the result by using the `message` function.

example
```emacs-lisp
(message "%s" result)
```

*** ELISP Code Examples:

#+begin_src emacs-lisp
;; Retrieve a list of buffers
(with-current-buffer (list-buffers-noselect)
 (message "%s" (buffer-substring-no-properties (point-min) (point-max))))
#+end_src

#+begin_src emacs-lisp
;; Read the contents of a specific buffer by name
(with-current-buffer \"{buffer-name}\" ;; Replace {buffer-name} with the actual buffer name
  (message "%s" (buffer-substring-no-properties (point-min) (point-max))))
#+end_src
"""


# --- State Definition ---

class AgentState(TypedDict):
    """The state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]


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

class ElispCode(BaseModel):
    code: str = Field(description="The Emacs Lisp code to execute. It must print its result to be captured.")

@tool(args_schema=ElispCode)
def execute_elisp_code(code: str) -> str:
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

# File System Tools
class ReadFileArgs(BaseModel):
    file_path: str = Field(description="The path to the file to read.")

class WriteToFileArgs(BaseModel):
    file_path: str = Field(description="The path to the file to write to.")
    content: str = Field(description="The content to write to the file.")

class ListFilesArgs(BaseModel):
    path: str = Field(description="The directory path to list files from. Defaults to the current directory.", default=".")

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

@tool(args_schema=WriteToFileArgs)
def write_to_file(file_path: str, content: str) -> str:
    """Writes the given content to a specified file, overwriting it if it exists."""
    try:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        return f"Successfully wrote to {file_path}"
    except Exception as e:
        return f"An unexpected error occurred while writing to the file: {str(e)}"

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


# --- Agent Definition ---

def _format_and_print_message(message: BaseMessage):
    """Formats and prints a message based on its type and role."""
    if hasattr(message, "tool_calls") and message.tool_calls:
        function_name = message.tool_calls[0]["name"]
        function_args = message.tool_calls[0]["args"]
        print(f"\n\033[1;33mCalling tool: {function_name} with args: {function_args}\033[0m")

    elif hasattr(message, "role") and message.role == "tool":
        print(f"\n\033[1;32mTool Result:\n{message.content}\033[0m")

    else:  # Assistant message
        content = message.content
        if isinstance(content, list):
            content = "".join(str(part) for part in content if part)
        if not isinstance(content, str):
            content = str(content)

        pattern = r"```(\w*)\n(.*?)```"
        matches = list(re.finditer(pattern, content, re.DOTALL))

        if not matches:
            print(f"\n\033[1;32mAssistant:\n{content}\033[0m")
            return

        print("\n\033[1;32mAssistant: [0m")
        last_end = 0
        for match in matches:
            pre_code_text = content[last_end:match.start()].strip()
            if pre_code_text:
                print(pre_code_text)

            language = match.group(1).strip()
            code_content = match.group(2).strip()
            lang_display = f" {language.capitalize()} Code " if language else " Code "
            header = f"---{lang_display}---"

            print(f"\n\033[1;36m{header}\033[0m")
            print(f"\033[36m{code_content}\033[0m")
            print(f"\033[1;36m{'-' * len(header)}\033[0m")

            last_end = match.end()

        post_code_text = content[last_end:].strip()
        if post_code_text:
            print(f"\n{post_code_text}")

class EmacsAgent:
    """Encapsulates the agent's logic, tools, and execution graph."""
    def __init__(self):
        if not GOOGLE_API_KEY:
            raise ValueError("GOOGLE_API_KEY environment variable not set.")

        self.llm = ChatGoogleGenerativeAI(
            model=MODEL_NAME,
            thinking_budget=THINKING_BUDGET,
            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,
            transport="grpc",
        )

        self.tools = [execute_elisp_code, read_file, write_to_file, list_files, grep, find_files]
        self.tools_by_name = {tool.name: tool for tool in self.tools}
        self.graph = self._build_graph()
        self.system_prompt = SYSTEM_PROMPT
        self.conversation_history = [SystemMessage(content=self.system_prompt)]

    def _build_graph(self) -> StateGraph:
        """Builds and compiles the LangGraph execution graph."""
        workflow = StateGraph(AgentState)
        workflow.add_node("llm", self.call_model)
        workflow.add_node("tools", self.call_tool)
        workflow.set_entry_point("llm")
        workflow.add_conditional_edges(
            "llm",
            self.should_continue,
            {"continue": "tools", "end": END},
        )
        workflow.add_edge("tools", "llm")
        return workflow.compile()

    def should_continue(self, state: AgentState) -> str:
        """Determines whether the agent should continue or end."""
        last_msg = state["messages"][-1]
        if hasattr(last_msg, "tool_calls") and last_msg.tool_calls:
            return "continue"
        return "end"

    def call_model(self, state: AgentState, config: RunnableConfig):
        """Invokes the LLM with the current state and tools."""
        model_with_tools = self.llm.bind_tools(self.tools)
        response = model_with_tools.invoke(state["messages"], config)
        return {"messages": [response]}

    def call_tool(self, state: AgentState):
        """Calls the appropriate tool based on the LLM's request."""
        outputs = []
        last_message = state["messages"][-1]

        if hasattr(last_message, "tool_calls") and last_message.tool_calls:
            for tool_call in last_message.tool_calls:
                tool_result = self.tools_by_name[tool_call["name"]].invoke(tool_call["args"])
                # Ensure tool_result is not empty
                if not tool_result:
                    tool_result = "Tool returned no output."
                outputs.append(
                    ToolMessage(
                        content=str(tool_result),
                        name=tool_call["name"],
                        tool_call_id=tool_call["id"],
                    )
                )
        return {"messages": outputs}

    def run(self, query: str):
        """Runs the agent from an initial user query."""
        self.conversation_history.append(HumanMessage(content=query))
        initial_state = {"messages": self.conversation_history.copy()}
        initial_history_length = len(self.conversation_history)
        final_messages = []

        for event in self.graph.stream(initial_state, stream_mode="values"):
            latest_message = event["messages"][-1]
            final_messages = event["messages"]
            if not isinstance(latest_message, HumanMessage):
                _format_and_print_message(latest_message)

        if len(final_messages) > initial_history_length:
            new_messages = final_messages[initial_history_length:]
            self.conversation_history.extend(new_messages)

    def clear_history(self):
        """Clear the conversation histories"""
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        print("Cleared conversation histories")

    def show_history(self):
        print("\n=== Conversation Histories ===")
        for i, msg in enumerate(self.conversation_history):
            role = getattr(msg, "role", msg.__class__.__name__)
            print(f"{i+1}. [{role}]: {msg.content[:100]}{'...' if len(msg.content) > 100 else ''}")
        print("==============================\n")


def main():
    """Main function to run the Emacs Agent."""
    print("Welcome to the Emacs Agent!")
    print("You can ask me to perform tasks in Emacs.")
    print("Special commands:")
    print("  - 'exit' or 'quit': end the session")
    print("  - 'clear': clear conversation history")
    print("  - 'history': show conversation history")

    try:
        agent = EmacsAgent()
        while True:
            query = input("\n> ")
            if query.lower() in ["exit", "quit"]:
                break
            elif query.lower() == "clear":
                agent.clear_history()
                continue
            elif query.lower() == "history":
                agent.show_history()
                continue
            agent.run(query)
    except ValueError as e:
        print(f"\n\033[1;31mInitialization Error: {e}\033[0m")
    except Exception as e:
        print(f"\n\033[1;31mAn unexpected error occurred: {e}\033[0m")

if __name__ == "__main__":
    # To run this script:
    # 1. Make sure you have an Emacs server running (`M-x server-start` in Emacs).
    # 2. Set your Google API key: `export GOOGLE_API_KEY='your_key_here'`
    # 3. Run the script: `python emacsagent.py`
    #
    # Example Query: "List all open buffers in Emacs."
    main()
