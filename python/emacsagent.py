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
from langgraph.checkpoint.memory import MemorySaver
from langgraph.graph.message import add_messages

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
MODEL_NAME = "gemini-2.5-flash"
LLM_TEMPERATURE = 0
TIMEOUT = 60
MEMORY_WINDOW_SIZE = 10  # Keep last 10 messages in active memory
MAX_EXECUTION_LOG_SIZE = 50  # Keep last 50 execution steps
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

The plan must be presented as a Markdown bulleted list.
Each bullet point should follow this format:
- **Step N: [Description]**
  Tool: `tool_name(arguments)`
  If Emacs Lisp code is required, include it using `execute_elisp_code(code)`.

Available tools for the junior developer:
- `list_files(path: str) -> str`
- `read_file(file_path: str) -> str`
- `write_to_file(file_path: str, content: str) -> str`
- `find_files(name_pattern: str, path: str = ".", file_type: str = None) -> str`
- `grep(pattern: str, path: str = ".", ignore_case: bool = False) -> str`
- `execute_elisp_code(code: str) -> str`

Requirements:
1. Always use Markdown bullets (`- `) for the plan.
2. Include “Step N: Description” in each bullet.
3. Specify the tool and its arguments in code format.
4. If the user’s request is unclear, output a clarification question.
5. If the request is too large, break it down into smaller, manageable steps.

Example Plan:
- **Step 1: List files in the current directory**
  Tool: `list_files(path=".")`
- **Step 2: Read the content of `init.el`**
  Tool: `read_file(file_path="init.el")`
- **Step 3: Add a new setting to `init.el`**
  Tool: `write_to_file(file_path="init.el", content="(setq my-new-setting t)")`
- **Step 4: Confirm with the user that the changes meet their requirements**
  _(This step does not require any tool.)_
"""

REFLECTION_PROMPT = """
You are a senior software engineer reviewing the agent's work.

Compare the agent’s last action against the original plan steps that were laid out.

- If the last action correctly implements the corresponding step from the plan and moves the user's request toward completion, respond with "SUCCESS".
- If the action deviates from, omits, or partially fulfills the planned step, provide brief, specific feedback on what needs to be corrected or added to align with the original plan.

Keep your feedback constructive, actionable, and tied directly to the plan's requirements.
"""

def fix_gemini_conversation_flow(messages: Sequence[BaseMessage]) -> Sequence[BaseMessage]:
    """
    Fix conversation flow to meet Gemini's requirements:
    - Function calls must come immediately after user messages or function responses
    - No consecutive AI messages
    - Proper alternating pattern
    """
    if not messages:
        return messages

    fixed_messages = []
    i = 0

    while i < len(messages):
        current_msg = messages[i]

        # Always keep system messages
        if isinstance(current_msg, SystemMessage):
            fixed_messages.append(current_msg)
            i += 1
            continue

        # Handle AI messages
        if isinstance(current_msg, AIMessage):
            # Check if this AI message has tool calls
            has_tool_calls = hasattr(current_msg, 'tool_calls') and current_msg.tool_calls

            # If AI message has tool calls, it must follow a user message or tool message
            if has_tool_calls:
                # Check if previous message is appropriate
                if fixed_messages:
                    prev_msg = fixed_messages[-1]
                    if not isinstance(prev_msg, (HumanMessage, ToolMessage, SystemMessage)):
                        # Insert a dummy user acknowledgment
                        fixed_messages.append(HumanMessage(content="Continue with the task."))

                fixed_messages.append(current_msg)
            else:
                # Regular AI message without tool calls
                # Check if we have consecutive AI messages
                if fixed_messages and isinstance(fixed_messages[-1], AIMessage):
                    # Merge consecutive AI messages
                    prev_content = fixed_messages[-1].content or ""
                    current_content = current_msg.content or ""
                    merged_content = f"{prev_content}\n\n{current_content}".strip()
                    fixed_messages[-1] = AIMessage(content=merged_content)
                else:
                    fixed_messages.append(current_msg)

        # Handle other message types normally
        elif isinstance(current_msg, (HumanMessage, ToolMessage)):
            fixed_messages.append(current_msg)

        i += 1

    return fixed_messages

def validate_gemini_conversation(messages: Sequence[BaseMessage]) -> bool:
    """Validate that the conversation follows Gemini's expected pattern."""
    for i in range(1, len(messages)):
        current_msg = messages[i]
        prev_msg = messages[i-1]

        # Check if AI message with tool calls follows appropriate message
        if isinstance(current_msg, AIMessage) and hasattr(current_msg, 'tool_calls') and current_msg.tool_calls:
            if not isinstance(prev_msg, (HumanMessage, ToolMessage, SystemMessage)):
                return False

    return True

def trim_messages_window(
    messages: Sequence[BaseMessage],
    window_size: int = MEMORY_WINDOW_SIZE
) -> Sequence[BaseMessage]:
    """
    Trim messages to keep only the most recent ones within the window size.
    Always preserve the system message if it exists, and never split
    a user→function_call pair.
    """
    # If we're already short enough, nothing to do
    if len(messages) <= window_size:
        return messages

    # Pull off the system message (if any) so we don't accidentally drop it
    has_system = isinstance(messages[0], SystemMessage)
    system_msg = messages[0] if has_system else None
    convo = list(messages[1:]) if has_system else list(messages)

    # We'll collect backwards, making absolutely sure we never cut away
    # the user message that triggered a function call
    keep: list[BaseMessage] = []
    needed = window_size - (1 if has_system else 0)

    # Walk backwards through convo
    i = len(convo) - 1
    while i >= 0 and len(keep) < needed:
        msg = convo[i]
        keep.append(msg)

        # If this is a function‐call, also pull in the turn before it
        if isinstance(msg, ToolMessage) and i - 1 >= 0:
            keep.append(convo[i - 1])
            # and skip that one since we've already grabbed it
            i -= 1

        i -= 1

    # Now reverse back into chronological order
    trimmed = list(reversed(keep))

    # Finally prepend our system message, if we had one
    if has_system:
        return [system_msg] + trimmed  # type: ignore
    else:
        return trimmed

def get_message_memory_size(messages: Sequence[BaseMessage]) -> int:
    """Calculate approximate memory usage of messages in characters."""
    total_chars = 0
    for msg in messages:
        if hasattr(msg, 'content') and msg.content:
            total_chars += len(str(msg.content))
    return total_chars


# --- State Definition ---

class AgentState(TypedDict):
    """The state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]
    n_reflection: int
    total_tokens_used: int = 0
    initial_plan: str = None # Added: Store the initial plan as a string


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

        self.memory = MemorySaver()

        self.tools = [execute_elisp_code, read_file, write_to_file, list_files, grep, find_files]
        self.tools_by_name = {tool.name: tool for tool in self.tools}

        # Build graph with memory checkpointer
        self.graph = self._build_graph().compile(checkpointer=self.memory)

        # Thread/session management
        self.thread_id = "emacs_agent_session"
        self.config = RunnableConfig(configurable={"thread_id": self.thread_id})

        self.system_prompt = SYSTEM_PROMPT
        self.plan_prompt = PLAN_PROMPT
        self.reflection_prompt = REFLECTION_PROMPT
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        self.current_node = None  # Track current node for message formatting
        self.execution_log = []  # Track execution steps

        # Memory optimization settings
        self.memory_window_size = MEMORY_WINDOW_SIZE
        self.max_execution_log_size = MAX_EXECUTION_LOG_SIZE

        # Initialize with system message
        self._initialize_memory()

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
            {"replan": "plan", "end": END},
        )
        return workflow

    def _initialize_memory(self):
        """Initialize memory with system message if not already present."""
        try:
            # Get the current state snapshot
            state = self.graph.get_state(self.config)

            # Check if we have any existing state and messages
            if not state or not state.values.get("messages"):
                # Initialize with system message
                initial_state = {
                    "messages": [SystemMessage(content=self.system_prompt)],
                    "n_reflection": 0,
                    "total_tokens_used": 0
                }
                self.graph.update_state(self.config, initial_state)
        except Exception as e:
            print(f"Warning: Could not initialize memory state: {e}")

    def _log_execution_step(self, node_name: str, action: str, details: str = ""):
        """Log execution steps for better debugging."""
        step = {
            "node": node_name,
            "action": action,
            "details": details,
            "timestamp": time.time()
        }
        self.execution_log.append(step)

        # Trim execution log if it gets too large
        if len(self.execution_log) > self.max_execution_log_size:
            # Keep only the most recent entries
            self.execution_log = self.execution_log[-self.max_execution_log_size:]
            print(f"\033[0;33m  (Trimmed execution log to {self.max_execution_log_size} entries)\033[0m")

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

        # Validate conversation flow before continuing
        if not validate_gemini_conversation(state["messages"]):
            print(f"\033[0;33m  Warning: Invalid conversation flow detected, fixing...\033[0m")
            # This will be handled in the next node call

        if hasattr(last_msg, "tool_calls") and last_msg.tool_calls:
            return "continue"
        return "end"

    def should_reflect(self, state: AgentState) -> str:
        """Determines whether the agent should reflect, replan, or end."""
        last_message = state["messages"][-1]

        # If the last message from reflection indicates success, end the process.
        if isinstance(last_message, AIMessage) and "SUCCESS" in last_message.content:
            self._log_execution_step("DECISION", "Reflection indicates success, ending execution")
            return "end"

        # If max reflections reached, end.
        if state["n_reflection"] > 3:
            self._log_execution_step("DECISION", "Max reflections reached, ending execution")
            return "end"

        # Otherwise, if reflection provided feedback, go back to planning.
        self._log_execution_step("DECISION", "Reflection provided feedback, re-planning")
        return "replan"

    def plan(self, state: AgentState):
        """Generates a plan to address the user's request."""
        self.current_node = "plan"
        self._log_execution_step("PLAN", "Generating execution plan")

        # Fix conversation flow before planning
        state_messages = fix_gemini_conversation_flow(state["messages"])
        state = {**state, "messages": state_messages}

        plan_message = [
            SystemMessage(content=self.plan_prompt),
            *state["messages"],
        ]

        # Trim plan messages if needed to avoid context overflow
        if len(plan_message) > self.memory_window_size + 1:  # +1 for plan prompt
            plan_message = [plan_message[0]] + list(plan_message[-(self.memory_window_size):])

        response = self.llm.invoke(plan_message)

        # Log the generated plan
        plan_summary = response.content[:100] + "..." if len(response.content) > 100 else response.content
        self._log_execution_step("PLAN", "Plan generated", plan_summary)

        # Store the generated plan in the state
        return {
            "messages": [AIMessage(content=response.content)],
            "initial_plan": response.content # Store the plan content
        }

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

    def reflect(self, state: AgentState):
        """Critiques the agent's last action and decides whether to continue."""
        self.current_node = "reflection"
        reflection_count = state["n_reflection"]
        self._log_execution_step("REFLECTION", f"Reflecting on execution (attempt {reflection_count + 1})")

        recent_messages = state["messages"]
        initial_plan = state.get("initial_plan", "No plan was generated or found in state.") # Retrieve the stored plan

        try:
            # Construct the reflection message, explicitly including the plan
            # This HumanMessage will guide the LLM to consider the plan
            reflection_message = [
                SystemMessage(content=self.reflection_prompt),
                *recent_messages,
                HumanMessage(content=f"Original Plan:\n```\n{initial_plan}\n```\n\nReview the conversation history and the execution log. Has the original plan been completed, and has the user's request been successfully addressed?"),
            ]

            # Validate conversation flow before continuing
            if not validate_gemini_conversation(reflection_message):
                print(f"\033[0;33m  Warning: Invalid conversation flow detected, fixing...\033[0m")

            # This will be handled in the next node call
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

    def run(self, query: str, thread_id: str = None):
        """Runs the agent from an initial user query with enhanced logging."""
        # Use provided thread_id or default
        if thread_id:
            self.thread_id = thread_id
            self.config = RunnableConfig(configurable={"thread_id": thread_id})

        print(f"\n\033[1;34mStarting execution for query: {query}\033[0m")
        self.execution_log.clear()  # Clear previous execution log

        # Get current state from memory
        current_state = self.graph.get_state(self.config)
        if current_state and current_state.values:
            existing_messages = current_state.values.get("messages", [])
            n_reflection = current_state.values.get("n_reflection", 0)
            initial_plan_from_state = current_state.values.get("initial_plan", None)
        else:
            existing_messages = [SystemMessage(content=self.system_prompt)]
            n_reflection = 0
            initial_plan_from_state = None

        # Add new human message
        new_messages = existing_messages + [HumanMessage(content=query)]
        initial_state = {
            "messages": new_messages,
            "n_reflection": 0,
            "total_tokens_used": 0,
            "initial_plan": initial_plan_from_state # Pass existing plan or None
        }
        initial_history_length = len(existing_messages)
        final_messages = []

        # Validate and fix conversation flow before starting
        initial_state["messages"] = fix_gemini_conversation_flow(initial_state["messages"])
        if not validate_gemini_conversation(initial_state["messages"]):
            print(f"\033[0;33m  Warning: Conversation flow still invalid after fixing\033[0m")

        try:
            # Track execution flow with node names
            for event in self.graph.stream(initial_state, config=self.config, stream_mode="values"):
                latest_message = event["messages"][-1]
                final_messages = event["messages"]

                # Only print non-human messages with node context
                if not isinstance(latest_message, HumanMessage):
                    _format_and_print_message(latest_message, self.current_node)

            # Update conversation history for backward compatibility
            if len(final_messages) > initial_history_length:
                new_messages = final_messages[initial_history_length:]
                self.conversation_history.extend(new_messages)

                # Trim conversation history to prevent unlimited growth
                self.conversation_history = trim_messages_window(self.conversation_history, self.memory_window_size * 2)

            print(f"\n\033[1;34mExecution completed. Total steps: {len(self.execution_log)}\033[0m")

        except Exception as e:
            print(f"\n\033[1;31mAn unexpected error occurred during agent execution: {e}\033[0m")
            traceback.print_exc()
            print("The agent will continue to be available for new requests.")

    def clear_history(self):
        """Clear the conversation histories and memory"""
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        self.execution_log.clear()

        # Clear memory state
        try:
            # Reset memory state
            initial_state = {
                "messages": [SystemMessage(content=self.system_prompt)],
                "n_reflection": 0,
                "total_tokens_used": 0,
                "initial_plan": None # Reset the plan
            }
            self.graph.update_state(self.config, initial_state, as_node="__start__")
            print("Cleared conversation histories, execution log, and memory state")
        except Exception as e:
            print(f"Cleared local history, but could not clear memory state: {e}")

    def debug_conversation_flow(self):
        """Debug conversation flow issues."""
        try:
            state = self.graph.get_state(self.config)
            if state and state.values:
                messages = state.values.get("messages", [])
                print(f"\n=== Conversation Flow Debug ===")
                for i, msg in enumerate(messages):
                    msg_type = type(msg).__name__
                    has_tools = hasattr(msg, 'tool_calls') and msg.tool_calls
                    tool_info = f" [HAS_TOOLS: {len(msg.tool_calls)}]" if has_tools else ""
                    content_preview = str(msg.content)[:50] + "..." if len(str(msg.content)) > 50 else str(msg.content)
                    print(f"{i+1:2d}. {msg_type:12}{tool_info}: {content_preview}")

                # Validate flow
                is_valid = validate_gemini_conversation(messages)
                print(f"\nConversation flow valid: {is_valid}")

                if not is_valid:
                    print("Suggested fix:")
                    fixed_messages = fix_gemini_conversation_flow(messages)
                    for i, msg in enumerate(fixed_messages):
                        msg_type = type(msg).__name__
                        has_tools = hasattr(msg, 'tool_calls') and msg.tool_calls
                        tool_info = f" [HAS_TOOLS: {len(msg.tool_calls)}]" if has_tools else ""
                        content_preview = str(msg.content)[:50] + "..." if len(str(msg.content)) > 50 else str(msg.content)
                        print(f"  {i+1:2d}. {msg_type:12}{tool_info}: {content_preview}")

                print("==============================\n")
        except Exception as e:
            print(f"Error debugging conversation flow: {e}")

    def get_memory_stats(self):
        """Get memory usage statistics."""
        try:
            state = self.graph.get_state(self.config)
            if state and state.values:
                messages = state.values.get("messages", [])
                memory_size = get_message_memory_size(messages)
                return {
                    "message_count": len(messages),
                    "memory_size_chars": memory_size,
                    "execution_log_size": len(self.execution_log),
                    "memory_window_size": self.memory_window_size
                }
        except Exception as e:
            print(f"Could not get memory stats: {e}")
        return {"error": "Could not retrieve memory statistics"}

    def show_history(self):
        """Display conversation history."""
        try:
            # Try to get from memory first
            state = self.graph.get_state(self.config)
            if state and state.values:
                messages = state.values.get("messages", [])
            else:
                messages = self.conversation_history

            print("\n=== Conversation History ===")
            for i, msg in enumerate(messages):
                role = getattr(msg, "role", msg.__class__.__name__)
                content_preview = str(msg.content)[:100] + "..." if len(str(msg.content)) > 100 else str(msg.content)
                print(f"{i+1:2d}. [{role:12}]: {content_preview}")

            # Show memory stats
            stats = self.get_memory_stats()
            print(f"Memory: {stats.get('message_count', 0)} messages, {stats.get('memory_size_chars', 0)} chars")
            print("============================\n")
        except Exception as e:
            print(f"Error showing history: {e}")

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
            print(f"{i:2d}. [{timestamp_str}] {step['node']:12} → {step['action']}\033[0m")
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
    print("  - 'memory': show memory usage statistics")

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
            elif query.lower() == "memory":
                stats = agent.get_memory_stats()
                print(f"\n=== Memory Statistics ===")
                for key, value in stats.items():
                    print(f"{key}: {value}")
                print("========================\n")
                continue
            elif query.lower() == "debug":
                agent.debug_conversation_flow()
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
