import os
import subprocess
import tempfile
import time
import re
import fnmatch
import datetime
import hashlib

from typing import Annotated, Sequence, TypedDict
from pydantic import BaseModel, Field

from langchain_core.messages import BaseMessage, ToolMessage, SystemMessage, HumanMessage
from langchain_core.runnables import RunnableConfig
from langchain_core.tools import tool
from langchain_google_genai import ChatGoogleGenerativeAI
from langgraph.graph import END, StateGraph
from langgraph.graph.message import add_messages

from memory import LongTermMemory
from prompt import SYSTEM_PROMPT, PRO_AGENT_SYSTEM_PROMPT

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
MAIN_MODEL_NAME = "gemini-2.5-flash"
PRO_MODEL_NAME = "gemini-2.5-pro"
LLM_TEMPERATURE = 0

TIMEOUT = 60

# --- Helper Function for Content Parsing ---

def _extract_text_from_content(content) -> str:
    """
    Extracts clean text from the message content, handling cases where
    Gemini returns a list of dictionaries (including extras/signatures).
    """
    if isinstance(content, list):
        text_parts = []
        for part in content:
            if isinstance(part, str):
                text_parts.append(part)
            elif isinstance(part, dict):
                # 辞書の場合は 'text' キーの中身を取り出す
                text_parts.append(part.get("text", ""))
        return "".join(text_parts)

    if not isinstance(content, str):
        return str(content)

    return content

# --- State Definition ---

class AgentState(TypedDict):
    """The state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]


# --- Base Agent Logic ---

class BaseAgent:
    """Base class for agent logic, tool handling, and graph construction."""
    def __init__(self, model_name: str, tools: list):
        if not GOOGLE_API_KEY:
            raise ValueError("GOOGLE_API_KEY environment variable not set.")

        self.llm = ChatGoogleGenerativeAI(
            model=model_name,
            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,
        )
        self.tools = tools
        self.tools_by_name = {tool.name: tool for tool in self.tools}
        self.graph = self._build_graph()
        self.system_prompt = SYSTEM_PROMPT

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


# --- Tools ---

def write_elisp_code_to_temp_file(code: str) -> str:
    """Create a temp file and write the provided code into it."""
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.el') as temp_file:
        temp_file.write(code)
        return temp_file.name.replace("\\", "/")

class ElispCode(BaseModel):
    code: str = Field(description="The Emacs Lisp code to execute. It must print its result to be captured.")

def _execute_elisp_code(code: str) -> str:
    temp_file_path = write_elisp_code_to_temp_file(code)
    command = f"emacsclient -e \"(hikizan/eval-elisp-file \\\"{temp_file_path}\\\")\""
    try:
        subprocess.run(command, shell=True, check=True, text=True, capture_output=True)
        time.sleep(1)
        temp_log_file_path = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.log').name.replace("\\", "/")
        log_command = f"emacsclient -e \"(hikizan/write-string-to-file \\\"{temp_log_file_path}\\\" (hikizan/get-string-from-point (get-buffer \\\"*Messages*\\\") (hikizan/find-string-position-in-buffer (get-buffer \\\"*Messages*\\\") \\\"{temp_file_path}\\\")))\""
        subprocess.run(log_command, shell=True, check=True, text=True, capture_output=True)
        with open(temp_log_file_path, 'r', encoding='utf-8') as log_file:
            return log_file.read().strip()
    except Exception as e:
        return f"Error: {str(e)}"

@tool(args_schema=ElispCode)
def execute_elisp_code(code: str) -> str:
    """Executes Emacs Lisp code and returns the result or an error message."""
    return _execute_elisp_code(code)

class WebPageURL(BaseModel):
    url: str = Field(description="The URL to browse.")

# --- Pro Agent for Consultation ---

class ProAgent(BaseAgent):
    """A specialized agent using the 'pro' model for complex tasks."""
    def __init__(self):
        pro_tools = []
        super().__init__(model_name=PRO_MODEL_NAME, tools=pro_tools)
        self.system_prompt = PRO_AGENT_SYSTEM_PROMPT

    def run(self, query: str) -> str:
        """Runs the agent for a single query and returns the final string response."""
        print(f"\n\033[1;35mConsulting Pro Agent for query: {query}\033[0m")
        initial_state = {"messages": [SystemMessage(content=self.system_prompt), HumanMessage(content=query)]}
        final_state = self.graph.invoke(initial_state)
        final_message = final_state["messages"][-1]

        # Use helper to extract clean text
        content = _extract_text_from_content(final_message.content)

        print(f"\n\033[1;35mPro Agent finished with response: {content}\033[0m")
        return content

class ProAgentQuery(BaseModel):
    query: str = Field(description="The query or task to delegate to the more powerful Pro model agent.")

@tool(args_schema=ProAgentQuery)
def consult_pro_agent(query: str) -> str:
    """
    Consults a more powerful agent (using gemini-2.5-pro) for complex reasoning or tasks.
    Use this when a task is too complex for the current model, requires deeper analysis, or involves multiple steps that need to be orchestrated.
    Returns the final response from the pro agent.
    """
    pro_agent = ProAgent()
    result = pro_agent.run(query)
    return result


# --- Main Emacs Agent ---

def _format_and_print_message(message: BaseMessage):
    """Formats and prints a message based on its type and role."""
    if hasattr(message, "tool_calls") and message.tool_calls:
        function_name = message.tool_calls[0]["name"]
        function_args = message.tool_calls[0]["args"]
        print(f"\n\033[1;33mCalling tool: {function_name} with args: {function_args}\033[0m")
    elif isinstance(message, ToolMessage):
        truncated_content = (message.content[:300] + "...") if len(message.content) > 300 else message.content
        print(f"\n\033[1;32mTool Result:\n{truncated_content}\033[0m")
    else:
        # Use helper to extract clean text
        content = _extract_text_from_content(message.content)
        print(f"\n\033[1;36mAssistant:\n{content}\033[0m")

class EmacsAgent(BaseAgent):
    """The main, interactive agent using the 'flash' model."""
    def __init__(self):
        main_tools = [execute_elisp_code, consult_pro_agent]
        super().__init__(model_name=MAIN_MODEL_NAME, tools=main_tools)
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        self.long_term_memory = LongTermMemory()

    def run(self, query: str):
        """Runs the agent interactively from a user query."""
        # 1. Retrieve relevant memories
        relevant_memories = self.long_term_memory.retrieve_relevant_memory(query, n_results=3)
        memory_context_str = self._format_memories_for_context(relevant_memories)

        # Add the current user query to history
        self.conversation_history.append(HumanMessage(content=query))

        # Prepare the initial state for the graph
        # Create a copy of the history to modify for the current invocation
        current_invocation_messages = self.conversation_history.copy()

        # Inject memory context if available, right after the initial SystemMessage
        # Assuming the first message in conversation_history is always the system prompt
        if memory_context_str:
            current_invocation_messages.insert(1, SystemMessage(content=memory_context_str))
            print(f"\n\033[1;34mDEBUG: Injected memory context into prompt:\n{memory_context_str[:300]}...\033[0m") # Optional: for debugging

        initial_state = {"messages": current_invocation_messages}
        # initial_history_length is used to track new messages added by the graph
        # It should reflect the length of self.conversation_history *before* the graph stream
        initial_history_length = len(self.conversation_history)
        final_messages = []

        for event in self.graph.stream(initial_state, stream_mode="values"):
            latest_message = event["messages"][-1]
            final_messages = event["messages"]
            if not isinstance(latest_message, HumanMessage):
                _format_and_print_message(latest_message)

        if len(final_messages) > initial_history_length:
            # New messages generated by the graph (tool calls, tool results, AI response)
            # These are the messages that were *not* in self.conversation_history before this run
            new_messages = final_messages[initial_history_length:]
            self.conversation_history.extend(new_messages)

            # 4. Add the interaction to long-term memory
            # The last message in final_messages should be the AI's final response
            # We need to find the actual AI response, not just a tool message.
            ai_response_content = ""
            for msg in reversed(new_messages):
                if isinstance(msg, BaseMessage) and msg.type == "ai": # Assuming 'ai' is the type for AI messages
                    # Use helper to extract clean text for memory storage
                    ai_response_content = _extract_text_from_content(msg.content)
                    break

            if ai_response_content:
                memory_to_add = f"User asked: '{query}'. Agent's final response was: '{ai_response_content}'"
                metadata = {
                    "timestamp": f"{datetime.datetime.now(datetime.UTC)}",
                    "type": "conversation_turn",
                    "user_query_hash": hashlib.sha256(query.encode('utf-8')).hexdigest(),
                    "response_length": len(ai_response_content)
                }
                print(f"\n\033[1;34mDEBUG: {metadata}\033[0m")
                self.long_term_memory.add_memory(memory_to_add, metadata=metadata)
                print(f"\n\033[1;34mDEBUG: Added interaction to long-term memory.\033[0m")
            else:
                print(f"\n\033[1;34mDEBUG: No AI response found to add to long-term memory.\033[0m")

    def clear_history(self):
        """Clears the conversation history."""
        self.conversation_history = [SystemMessage(content=self.system_prompt)]
        print("Cleared conversation histories")

    def show_history(self):
        """Displays the conversation history."""
        print("\n=== Conversation Histories ===")
        for i, msg in enumerate(self.conversation_history):
            role = getattr(msg, "role", msg.__class__.__name__)
            # Clean content before showing history
            clean_content = _extract_text_from_content(msg.content)
            print(f"{i+1}. [{role}]: {clean_content[:100]}{'...' if len(clean_content) > 100 else ''}")
        print("==============================\n")

    def _format_memories_for_context(self, memories: list[str]) -> str:
        if not memories:
            return ""
        formatted_memories = "\n".join(f"- {mem}" for mem in memories)
        return f"You have the following relevant memories from past interactions:\n{formatted_memories}\n"


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
            lines = []
            prompt = "\n>>> "
            while True:
                line = input(prompt)
                if not line: # Check if the line is empty
                    break
                lines.append(line)
                prompt = "    "
            query = "\n".join(lines)

            if query.strip().lower() in ["exit", "quit"]:
                break
            elif query.strip().lower() == "clear":
                agent.clear_history()
                continue
            elif query.strip().lower() == "history":
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
