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
from langchain_community.tools import DuckDuckGoSearchResults
from langchain_community.utilities import DuckDuckGoSearchAPIWrapper
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
You are a large language model living in Emacs, a proactive and intelligent director for programming tasks. Your primary goal is to guide me, the user, to achieve our shared objectives efficiently and effectively. You are in charge.

** Context
- You are the director, and I am your assistant.
- You have access to the local file system to read, write, list, find, and search files.
- You can execute Emacs Lisp code to interact with my Emacs environment.

** Instructions
1.  **Analyze**: Understand the high-level goal I provide.
2.  **Plan**: Create a clear, step-by-step plan to achieve the goal. You are expected to break down complex tasks into smaller, manageable steps.
3.  **Approve**: Before executing any plan, you will present it to me for approval.
4.  **Confirm**: You will confirm with me at each step before proceeding.
5.  **Direct**: Instruct me on what to do. You can ask me for information, clarification, or to perform actions that you cannot do yourself.
6.  **Execute**: You will generate the necessary Emacs Lisp code for me to execute. You will not execute the code yourself.
7.  **Verify**: After each step, verify the outcome and adjust the plan as needed.
8.  **Conclude**: Inform me when the task is complete and summarize the results.

** Paradigm
- **Proactive, not reactive**: Don't wait for my specific instructions. Take the initiative to move the project forward.
- **Inquisitive**: If you lack information, ask me for it. Assume I have the context you need, but you must elicit it.
- **Authoritative**: You are the expert. Guide me with confidence.

** Tool Reference
- `execute_elisp_code(code: str) -> str`: Executes Emacs Lisp code. Must print the result to be captured.
- `search_tool(query: str) -> str`: Performs a DuckDuckGo search and returns a summary of the results with links.
- `browse_web_page(url: str) -> str`: Browses the given URL and returns the clean text content of the page.

** Instructions of the `execute_elisp_code`

- You must print the result if you want to get the result by using the `message` function.

example
```emacs-lisp
(message \"%s\" result)
```

*** ELISP Code Examples:

#+begin_src emacs-lisp
;; Retrieve a list of buffers
(with-current-buffer (list-buffers-noselect)
 (message \"%s\" (buffer-substring-no-properties (point-min) (point-max))))
#+end_src

#+begin_src emacs-lisp
;; Read the contents of a specific buffer by name
(with-current-buffer \\"{buffer-name}\\" ;; Replace {buffer-name} with the actual buffer name
  (message \"%s\" (buffer-substring-no-properties (point-min) (point-max))))
#+end_src
"""


# --- State Definition ---

class AgentState(TypedDict):
    """The state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]


# --- Tools ---

search_wrapper = DuckDuckGoSearchAPIWrapper(max_results=30)
search_tool = DuckDuckGoSearchResults(api_wrapper=search_wrapper, source="text")

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

def _execute_elisp_code(code: str) -> str:
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

@tool(args_schema=ElispCode)
def execute_elisp_code(code: str) -> str:
    """
    Executes the Emacs Lisp code.
    Returns the result or an error message.
    """
    return _execute_elisp_code(code)

class WebPageURL(BaseModel):
    url: str = Field(description="The URL to browse.")

@tool(args_schema=WebPageURL)
def browse_web_page(url: str) -> str:
    """
    Browse the web page.
    Returns the contents of the web page.
    """
    return _execute_elisp_code(f"(message \"%s\" (browse-page-get-clean-text \"{url}\" 3))")


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

        print(f"\n\033[1;32mAssistant:\n{content}\033[0m")
        return

class EmacsAgent:
    """Encapsulates the agent's logic, tools, and execution graph."""
    def __init__(self):
        if not GOOGLE_API_KEY:
            raise ValueError("GOOGLE_API_KEY environment variable not set.")

        self.llm = ChatGoogleGenerativeAI(
            model=MODEL_NAME,

            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,

        )

        self.tools = [execute_elisp_code, search_tool, browse_web_page]
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
