# import packages
import os
import subprocess
import tempfile
import time
import re
import fnmatch
import traceback

from typing import Annotated, Sequence, TypedDict
from pydantic import BaseModel, Field

from langchain_core.messages import BaseMessage, ToolMessage, SystemMessage, HumanMessage, AIMessage
from langchain_core.runnables import RunnableConfig
from langchain_core.tools import tool
from langchain_google_genai import ChatGoogleGenerativeAI
from langgraph.graph import END, StateGraph
from langgraph.graph.message import add_messages

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
MODEL_NAME = "gemini-2.5-flash"
LLM_TEMPERATURE = 0
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

** Constraints
- You must not ask for any personal information.
- You must only use the provided tools to answer the user's request.
- You must provide your response in markdown format.

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

PLAN_PROMPT = """
You are a senior software engineer.
Your role is to craft a detailed, step-by-step plan to address the user's request.
The user's request is the last message in the conversation.

The plan should be a sequence of commands to be executed by a junior developer.
The junior developer has access to the following tools:
- `list_files(path: str) -> str`
- `read_file(file_path: str) -> str`
- `write_to_file(file_path: str, content: str) -> str`
- `find_files(name_pattern: str, path: str = ".", file_type: str = None) -> str`
- `grep(pattern: str, path: str = ".", ignore_case: bool = False) -> str`
- `execute_elisp_code(code: str) -> str`

Your plan should be clear, concise, and easy to follow.
For each step, specify the tool to be used and the arguments to be passed.
If you need to use `execute_elisp_code`, provide the Emacs Lisp code to be executed.

Example Plan:
1. **List files in the current directory**: `list_files(path=".")`
2. **Read the content of `init.el`**: `read_file(file_path="init.el")`
3. **Add a new setting to `init.el`**: `write_to_file(file_path="init.el", content="(setq my-new-setting t)")`

If the user's request is unclear, ask for clarification.
If the user's request is too complex, break it down into smaller, manageable steps.
"""

REFLECTION_PROMPT = """
You are a senior software engineer reviewing the agent's work.

Look at the agent's last action and determine if it successfully completed the user's request.

If the action was successful and fulfilled the user's request, respond with "SUCCESS".
If the action was not successful or has not yet fulfilled the user's request, provide brief, specific feedback on what needs to be improved.

Keep your feedback constructive and actionable.
"""


# --- State Definition ---

class AgentState(TypedDict):
    """The state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]
    n_reflection: int


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


# --- Message Formatting ---

def _format_and_print_message(message: BaseMessage, node_name: str = None):
    """Formats and prints a message based on its type and role, with optional node context."""

    # Create node prefix if provided
    node_prefix = f"\033[1;36m[{node_name.upper()}]\033[0m " if node_name else ""

    if hasattr(message, "tool_calls") and message.tool_calls:
        function_name = message.tool_calls[0]["name"]
        function_args = message.tool_calls[0]["args"]
        print(f"\n{node_prefix}\033[1;33mCalling tool: {function_name} with args: {function_args}\033[0m")

    elif hasattr(message, "role") and message.role == "tool":
        print(f"\n{node_prefix}\033[1;32mTool Result:\n{message.content}\033[0m")

    else:  # Assistant message
        content = message.content
        if isinstance(content, list):
            content = "".join(str(part) for part in content if part)
        if not isinstance(content, str):
            content = str(content)

        print(f"\n{node_prefix}\033[1;32mAssistant:\n{content}\033[0m")


# --- Emacs Agent Definition ---

class EmacsAgent:
    """Emacs Agent with detailed execution logging and node context tracking."""
    def __init__(self):
        if not GOOGLE_API_KEY:
            raise ValueError("GOOGLE_API_KEY environment variable not set.")

        self.llm = ChatGoogleGenerativeAI(
            model=MODEL_NAME,
            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,
        )

        self.tools = [execute_elisp_code, read_file, write_to_file, list_files, grep, find_files]
        self.tools_by_name = {tool.name: tool for tool in self.tools}
        self.graph = self._build_graph()
        self.system_prompt = SYSTEM_PROMPT
        self.plan_prompt = PLAN_PROMPT
        self.reflection_prompt = REFLECTION_PROMPT
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        self.current_node = None  # Track current node for message formatting
        self.execution_log = []  # Track execution steps

    def _build_graph(self) -> StateGraph:
        """Builds and compiles the LangGraph execution graph."""
        workflow = StateGraph(AgentState)
        workflow.add_node("plan", self.plan)
        workflow.add_node("llm", self.call_model)
        workflow.add_node("tools", self.call_tool)
        workflow.add_node("reflection", self.reflect)
        workflow.set_entry_point("plan")
        workflow.add_edge("plan", "llm")
        workflow.add_conditional_edges(
            "llm",
            self.should_continue,
            {"continue": "tools", "end": END},
        )
        workflow.add_edge("tools", "reflection")
        workflow.add_conditional_edges(
            "reflection",
            self.should_reflect,
            {"reflect": "llm", "end": END},
        )
        return workflow.compile()

    def _log_execution_step(self, node_name: str, action: str, details: str = ""):
        """Log execution steps for better debugging."""
        step = {
            "node": node_name,
            "action": action,
            "details": details,
            "timestamp": time.time()
        }
        self.execution_log.append(step)

        # Print execution step with enhanced formatting
        timestamp_str = time.strftime("%H:%M:%S", time.localtime(step["timestamp"]))
        print(f"\n\033[1;35m[{timestamp_str}] {node_name.upper()} → {action}\033[0m")
        if details:
            # Truncate long details for readability
            truncated_details = details[:100] + "..." if len(details) > 100 else details
            print(f"\033[0;35m  Details: {truncated_details}\033[0m")

    def should_continue(self, state: AgentState) -> str:
        """Determines whether the agent should continue or end."""
        last_msg = state["messages"][-1]
        if hasattr(last_msg, "tool_calls") and last_msg.tool_calls:
            return "continue"
        return "end"

    def should_reflect(self, state: AgentState) -> str:
        """Determines whether the agent should reflect or end."""
        if state["n_reflection"] > 3:
            self._log_execution_step("DECISION", "Max reflections reached, ending execution")
            return "end"
        return "reflect"

    def plan(self, state: AgentState):
        """Generates a plan to address the user's request."""
        self.current_node = "plan"
        self._log_execution_step("PLAN", "Generating execution plan")

        plan_message = [
            SystemMessage(content=self.plan_prompt),
            *state["messages"],
        ]
        response = self.llm.invoke(plan_message)

        # Log the generated plan
        plan_summary = response.content[:100] + "..." if len(response.content) > 100 else response.content
        self._log_execution_step("PLAN", "Plan generated", plan_summary)

        return {"messages": [AIMessage(content=response.content)]}

    def call_model(self, state: AgentState, config: RunnableConfig):
        """Invokes the LLM with the current state and tools."""
        self.current_node = "llm"
        self._log_execution_step("LLM", "Calling language model")

        model_with_tools = self.llm.bind_tools(self.tools)
        response = model_with_tools.invoke(state["messages"], config)

        # Log what the model decided to do
        if hasattr(response, "tool_calls") and response.tool_calls:
            tool_name = response.tool_calls[0]["name"]
            tool_args = response.tool_calls[0]["args"]
            self._log_execution_step("LLM", f"Decided to call tool: {tool_name}", str(tool_args))
        else:
            content_preview = response.content[:50] + "..." if len(response.content) > 50 else response.content
            self._log_execution_step("LLM", "Generated text response", content_preview)

        return {"messages": [response]}

    def call_tool(self, state: AgentState):
        """Calls the appropriate tool based on the LLM's request."""
        self.current_node = "tools"
        outputs = []
        last_message = state["messages"][-1]

        if hasattr(last_message, "tool_calls") and last_message.tool_calls:
            for tool_call in last_message.tool_calls:
                tool_name = tool_call["name"]
                tool_args = tool_call["args"]

                self._log_execution_step("TOOLS", f"Executing {tool_name}", str(tool_args))

                tool_result = self.tools_by_name[tool_name].invoke(tool_args)

                # Ensure tool_result is not empty
                if not tool_result:
                    tool_result = "Tool returned no output."

                # Log tool result summary
                result_summary = str(tool_result)[:100] + "..." if len(str(tool_result)) > 100 else str(tool_result)
                self._log_execution_step("TOOLS", f"{tool_name} completed", result_summary)

                outputs.append(
                    ToolMessage(
                        content=str(tool_result),
                        name=tool_name,
                        tool_call_id=tool_call["id"],
                    )
                )
        return {"messages": outputs}

    def validate_messages_for_gemini(self, messages):
        """Ensure all messages are compatible with Gemini API"""
        validated_messages = []

        for msg in messages:
            if isinstance(msg, SystemMessage):
                # Clean system message content
                content = msg.content.replace('\\"', '"').replace('\\n', '\n')
                validated_messages.append(SystemMessage(content=content))
            elif isinstance(msg, HumanMessage):
                # Keep human messages as-is
                validated_messages.append(HumanMessage(content=str(msg.content)))
            elif isinstance(msg, AIMessage):
                # Simplify AI messages - remove tool_calls metadata
                if hasattr(msg, 'tool_calls') and msg.tool_calls:
                    # Create a simplified description of the tool call
                    tool_name = msg.tool_calls[0]['name']
                    tool_args = msg.tool_calls[0]['args']
                    simplified_content = f"Called tool '{tool_name}' with arguments: {tool_args}"
                    if msg.content:
                        simplified_content = f"{msg.content}\n\n{simplified_content}"
                    validated_messages.append(AIMessage(content=simplified_content))
                else:
                    # Keep regular AI messages
                    validated_messages.append(AIMessage(content=str(msg.content or "")))
            elif isinstance(msg, ToolMessage):
                # Convert tool messages to human messages for clarity
                tool_result_msg = HumanMessage(content=f"Tool '{msg.name}' returned: {msg.content}")
                validated_messages.append(tool_result_msg)

        return validated_messages

    def reflect(self, state: AgentState):
        """Critiques the agent's last action and decides whether to continue."""
        self.current_node = "reflection"
        reflection_count = state["n_reflection"]
        self._log_execution_step("REFLECTION", f"Reflecting on execution (attempt {reflection_count + 1})")

        # Get recent messages to avoid overwhelming the context
        recent_messages = state["messages"][-6:]  # Last 6 messages

        # Validate and simplify messages for Gemini
        validated_messages = self.validate_messages_for_gemini(recent_messages)

        reflection_message = [
            SystemMessage(content=self.reflection_prompt),
            *validated_messages,
        ]

        try:
            response = self.llm.invoke(reflection_message)

            if "SUCCESS" in response.content:
                self._log_execution_step("REFLECTION", "Task completed successfully")
                return {"messages": [AIMessage(content="SUCCESS")]}
            else:
                feedback_preview = response.content[:100] + "..." if len(response.content) > 100 else response.content
                self._log_execution_step("REFLECTION", "Needs improvement", feedback_preview)
                return {
                    "messages": [AIMessage(content=response.content)],
                    "n_reflection": state["n_reflection"] + 1
                }

        except Exception as e:
            self._log_execution_step("REFLECTION", "Reflection failed", str(e))
            print(f"\n\033[1;31mReflection error: {e}\033[0m")
            traceback.print_exc()
            # Return a safe fallback to prevent the entire process from failing
            return {
                "messages": [AIMessage(content="CONTINUE: Reflection failed, proceeding with execution.")],
                "n_reflection": state["n_reflection"] + 1
            }

    def run(self, query: str):
        """Runs the agent from an initial user query with enhanced logging."""
        print(f"\n\033[1;34mStarting execution for query: {query}\033[0m")
        self.execution_log.clear()  # Clear previous execution log

        self.conversation_history.append(HumanMessage(content=query))
        initial_state = {"messages": self.conversation_history.copy(), "n_reflection": 0}
        initial_history_length = len(self.conversation_history)
        final_messages = []

        try:
            # Track execution flow with node names
            for event in self.graph.stream(initial_state, stream_mode="values"):
                latest_message = event["messages"][-1]
                final_messages = event["messages"]

                # Only print non-human messages with node context
                if not isinstance(latest_message, HumanMessage):
                    _format_and_print_message(latest_message, self.current_node)

            if len(final_messages) > initial_history_length:
                new_messages = final_messages[initial_history_length:]
                self.conversation_history.extend(new_messages)

            print(f"\n\033[1;34mExecution completed. Total steps: {len(self.execution_log)}\033[0m")

        except Exception as e:
            print(f"\n\033[1;31mAn unexpected error occurred during agent execution: {e}\033[0m")
            traceback.print_exc()
            print("The agent will continue to be available for new requests.")

    def clear_history(self):
        """Clear the conversation histories"""
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        self.execution_log.clear()
        print("Cleared conversation histories and execution log")

    def show_history(self):
        """Display conversation history."""
        print("\n=== Conversation History ===")
        for i, msg in enumerate(self.conversation_history):
            role = getattr(msg, "role", msg.__class__.__name__)
            content_preview = str(msg.content)[:100] + "..." if len(str(msg.content)) > 100 else str(msg.content)
            print(f"{i+1:2d}. [{role:12}]: {content_preview}")
        print("============================\n")

    def show_execution_log(self):
        """Display the execution log for debugging."""
        if not self.execution_log:
            print("\n=== Execution Log ===")
            print("No execution steps recorded.")
            print("====================\n")
            return

        print("\n=== Execution Log ===")
        for i, step in enumerate(self.execution_log, 1):
            timestamp_str = time.strftime("%H:%M:%S", time.localtime(step["timestamp"]))
            print(f"{i:2d}. [{timestamp_str}] {step['node']:12} → {step['action']}")
            if step["details"]:
                # Truncate long details
                details = step["details"][:150] + "..." if len(step["details"]) > 150 else step["details"]
                print(f"     {details}")
        print("====================\n")

    def show_stats(self):
        """Show execution statistics."""
        if not self.execution_log:
            print("No execution data available.")
            return

        node_counts = {}
        for step in self.execution_log:
            node = step["node"]
            node_counts[node] = node_counts.get(node, 0) + 1

        total_time = 0
        if len(self.execution_log) > 1:
            total_time = self.execution_log[-1]["timestamp"] - self.execution_log[0]["timestamp"]

        print(f"\n=== Execution Statistics ===")
        print(f"Total steps: {len(self.execution_log)}")
        print(f"Total time: {total_time:.2f} seconds")
        print(f"Node usage:")
        for node, count in sorted(node_counts.items()):
            print(f"  {node:12}: {count} steps")
        print("===========================\n")


def main():
    """Main function to run the Emacs Agent."""
    print("Welcome to the Emacs Agent!")
    print("You can ask me to perform tasks in Emacs.")
    print("Special commands:")
    print("  - 'exit' or 'quit': end the session")
    print("  - 'clear': clear conversation history and execution log")
    print("  - 'history': show conversation history")
    print("  - 'log': show execution log")
    print("  - 'stats': show execution statistics")

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
            elif query.lower() == "log":
                agent.show_execution_log()
                continue
            elif query.lower() == "stats":
                agent.show_stats()
                continue
            agent.run(query)
    except ValueError as e:
        print(f"\n\033[1;31mInitialization Error: {e}\033[0m")
        traceback.print_exc()
    except Exception as e:
        print(f"\n\033[1;31mAn unexpected error occurred in the main loop: {e}\033[0m")
        traceback.print_exc()

if __name__ == "__main__":
    # To run this script:
    # 1. Make sure you have an Emacs server running (`M-x server-start` in Emacs).
    # 2. Set your Google API key: `export GOOGLE_API_KEY='your_key_here'`
    # 3. Run the script: `python emacsagent.py`
    #
    # Example Query: "List all open buffers in Emacs."
    main()
