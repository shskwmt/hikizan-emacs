# import packages
import os
import subprocess
import tempfile
import time
import re

from typing import Annotated, Sequence, TypedDict

from langchain_core.messages import BaseMessage, ToolMessage, SystemMessage, HumanMessage
from langchain_core.runnables import RunnableConfig
from langchain_core.tools import tool
from langchain_google_genai import ChatGoogleGenerativeAI
from langgraph.graph import END, StateGraph
from langgraph.graph.message import add_messages

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
MODEL_NAME = "gemini-2.5-pro-preview-06-05"
LLM_TEMPERATURE = 0  # Lowered for more predictable code-related outputs
SYSTEM_PROMPT = """
** Role
You are a large language model living in Emacs and helpful assistant.

** Context
- I am using emacs to automate programming tasks.

** Instructions
- Write efficient ELISP code to perform the objective.
- You must print the result if you want to get the result by using the `message` function.

example
```emacs-lisp
(message "%s" result)
```

** ELISP Code Examples:
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

def write_elisp_code_to_temp_file(code: str) -> str:
    """
    Create a temp file and write the provided code into the temp file.
    Returns the temp file path.

    Parameters:
    code (str): Emacs Lisp code to write.

    Returns:
    str: temp file path.
    """
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.el') as temp_file:
        temp_file.write(code)
        file_path = temp_file.name.replace("\\", "/")
        return file_path

@tool("execute_elisp_code", return_direct=True)
def execute_elisp_code(code: str) -> str:
    """
    Executes the Emacs Lisp code.
    Returns the result or an error message.

    Parameters:
    code (str): Emacs Lisp code to execute.

    Returns:
    str: the result or an error message
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


# --- Agent Definition ---

def _format_and_print_message(message: BaseMessage):
    """Formats and prints a message based on its type and role."""
    if hasattr(message, "tool_calls") and message.tool_calls:
        # The model is asking to call a tool
        function_name = message.tool_calls[0]["name"]
        function_args = message.tool_calls[0]["args"]
        print(f"\n\033[1;33mCalling tool: {function_name} with args: {function_args}\033[0m")

    elif hasattr(message, "role") and message.role == "tool":
        # A tool has been called and returned a result
        print(f"\n\033[1;32mTool Result:\n{message.content}\033[0m")

    else:  # Assistant message
        content = message.content
        # Ensure content is a string before proceeding.
        if isinstance(content, list):
            # If content is a list (e.g., from multi-part messages), join it.
            content = "".join(str(part) for part in content if part)

        if not isinstance(content, str):
            content = str(content)  # Fallback for other non-string types

        # Use a regex to find all code blocks, including the language identifier
        pattern = r"```(\w*)\n(.*?)```"
        matches = list(re.finditer(pattern, content, re.DOTALL))

        if not matches:
            # No code blocks found, print as is.
            print(f"\n\033[1;32mAssistant:\n{content}\033[0m")
            return

        print("\n\033[1;32mAssistant: [0m")
        last_end = 0
        for match in matches:
            # Print the text before the current code block
            pre_code_text = content[last_end:match.start()].strip()
            if pre_code_text:
                print(pre_code_text)

            # Extract language and code
            language = match.group(1).strip()
            code_content = match.group(2).strip()

            # Format the header for the code block
            lang_display = f" {language.capitalize()} Code " if language else " Code "
            header = f"---{lang_display}---"

            # Print the formatted code block
            print(f"\n\033[1;36m{header}\033[0m")
            print(f"\033[36m{code_content}\033[0m")
            print(f"\033[1;36m{'-' * len(header)}\033[0m")

            last_end = match.end()

        # Print any remaining text after the last code block
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
            temperature=LLM_TEMPERATURE,
            google_api_key=GOOGLE_API_KEY
        )
        self.tools = [execute_elisp_code]
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
        # Bind the tools to the LLM so it knows what functions it can call
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

        initial_state = {
            "messages": self.conversation_history.copy()
        }

        initial_history_length = len(self.conversation_history)
        final_messages = []

        # The `stream` method provides real-time updates on the state
        for event in self.graph.stream(initial_state, stream_mode="values"):
            # `event` is the entire state. Get the latest message to print.
            latest_message = event["messages"][-1]
            final_messages = event["messages"]

            # Format and print the latest message from the stream.
            # The stream yields state updates, so this prints the result of each step.
            # We check the message is not the initial human message of the turn.
            if not isinstance(latest_message, HumanMessage):
                _format_and_print_message(latest_message)

        # After the stream is complete, update the conversation history
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
            if hasattr(msg, "role"):
                role = msg.role
            else:
                role = msg.__class__.__name__
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
