import os
import time
import re
import traceback
import json

from typing import Annotated, Sequence, TypedDict, List, Optional

from langchain_core.messages import BaseMessage, SystemMessage, HumanMessage, AIMessage
from langchain_core.runnables import RunnableConfig
from langchain_google_genai import ChatGoogleGenerativeAI
from langgraph.graph import END, StateGraph
from langgraph.checkpoint.memory import MemorySaver
from langgraph.graph.message import add_messages

from tools import execute_elisp_code, read_buffer, read_file, write_to_file, list_files, grep, find_files
from prompt import SYSTEM_PROMPT, PLAN_PROMPT, STEP_EXECUTION_PROMPT, REFLECTION_PROMPT

# --- Constants ---

GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
PLANNER_MODEL_NAME = "gemini-2.5-flash"
EXECUTOR_MODEL_NAME = "gemini-2.5-flash"
LLM_TEMPERATURE = 0.3
TIMEOUT = 60
MAX_EXECUTION_LOG_SIZE = 50

# --- State Definition ---

class PlanStep(TypedDict):
    step_number: int
    description: str
    tool: str
    estimated_time: str
    status: str  # "pending", "in_progress", "completed", "failed"
    result: Optional[str]
    feedback: Optional[str]

class AgentState(TypedDict):
    """The enhanced state of the agent."""
    messages: Annotated[Sequence[BaseMessage], add_messages]
    original_request: str
    previous_plan: str
    feedback_for_plan: str
    plan_steps: List[PlanStep]
    current_step_index: int
    step_context: str

# --- Utility Functions ---
def step_to_text(step: PlanStep, step_index: int, current_step_index: int):
    status_icon = "✓" if step["status"] == "completed" else "→" if step_index == current_step_index else "○"
    step_text = f"** {status_icon} Step {step['step_number']}:** {step['description']}\n"
    step_text += f"- Tool: `{step['tool']}`\n"
    step_text += f"- Estimated time: {step['estimated_time']}\n"
    step_text += f"- Status: {step['status']}\n"
    if step.get("result"):
        result_preview = step["result"][:100] + "..." if len(step["result"]) > 100 else step["result"]
        step_text += f"- Result: {result_preview}\n"
    step_text += "\n"
    return step_text

def plan_to_text(plan_steps: List[PlanStep], current_step_index: int):
    plan = "=== Current Plan ===\n"
    for i, step in enumerate(plan_steps):
        plan += step_to_text(step, i, current_step_index)
    plan += "==================\n\n"

    return plan

def summarize_step_results(step_results: List[PlanStep]):
    results = []
    for step in step_results:
        results.append({
            "step": step["step_number"],
            "description": step["description"],
            "status": step["status"],
            "result": step.get("result", "No result recorded")
        })

    return json.dumps(results, indent=2)

# --- Emacs Agent ---

class EmacsAgent:
    """Enhanced Emacs Agent with user feedback and step-by-step execution."""

    def __init__(self):
        if not GOOGLE_API_KEY:
            raise ValueError("GOOGLE_API_KEY environment variable not set.")

        self.planner_llm = ChatGoogleGenerativeAI(
            model=PLANNER_MODEL_NAME,
            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,
        )

        self.executor_llm = ChatGoogleGenerativeAI(
            model=EXECUTOR_MODEL_NAME,
            temperature=LLM_TEMPERATURE,
            timeout=TIMEOUT,
            google_api_key=GOOGLE_API_KEY,
        )

        self.memory = MemorySaver()
        self.tools = [execute_elisp_code, read_buffer, read_file, write_to_file, list_files, grep, find_files]
        self.tools_by_name = {tool.name: tool for tool in self.tools}

        # Build graph with memory checkpointer
        self.graph = self._build_graph().compile(checkpointer=self.memory)

        # Thread/session management
        self.thread_id = "enhanced_emacs_agent_session"
        self.config = RunnableConfig(configurable={"thread_id": self.thread_id})

        self.system_prompt = SYSTEM_PROMPT
        self.execution_log = []

    def _build_graph(self) -> StateGraph:
        """Builds the enhanced execution graph with user feedback loops."""
        workflow = StateGraph(AgentState)

        # Add all nodes
        workflow.add_node("plan", self.generate_plan)
        workflow.add_node("wait_plan_review", self.wait_for_plan_review)
        workflow.add_node("execute_step", self.execute_current_step)
        workflow.add_node("wait_step_feedback", self.wait_for_step_feedback)
        workflow.add_node("increment_step_index", self.increment_step_index)
        workflow.add_node("reflect", self.reflect_on_completion)
        workflow.add_node("wait_final_review", self.wait_for_final_review)

        # Set entry point
        workflow.set_entry_point("plan")

        # Add edges
        workflow.add_edge("plan", "wait_plan_review")
        workflow.add_conditional_edges(
            "wait_plan_review",
            self.plan_review_decision,
            {
                "approved": "execute_step",
                "needs_revision": "plan",
                "waiting": "wait_plan_review",
                "exit": END
            }
        )
        workflow.add_edge("execute_step", "wait_step_feedback")
        workflow.add_conditional_edges(
            "wait_step_feedback",
            self.step_feedback_decision,
            {
                "continue": "increment_step_index",
                "redo": "execute_step",
                "completed": "reflect",
                "waiting": "wait_step_feedback"
            }
        )
        workflow.add_conditional_edges(
            "increment_step_index",
            self.plan_review_decision,
            {
                "approved": "execute_step",
                "waiting": "execute_step",
                "needs_revision": "plan",
                "exit": END
            }
        )
        workflow.add_edge("reflect", "wait_final_review")
        workflow.add_conditional_edges(
            "wait_final_review",
            self.final_review_decision,
            {
                "success": END,
                "replan": "plan",
                "waiting": "wait_final_review"
            }
        )

        return workflow

    def _log_execution_step(self, node_name: str, action: str, details: str = ""):
        """Log execution steps for better debugging."""
        step = {
            "node": node_name,
            "action": action,
            "details": details,
            "timestamp": time.time()
        }
        self.execution_log.append(step)

        if len(self.execution_log) > MAX_EXECUTION_LOG_SIZE:
            self.execution_log = self.execution_log[-MAX_EXECUTION_LOG_SIZE:]

        timestamp_str = time.strftime("%H:%M:%S", time.localtime(step["timestamp"]))
        print(f"\n\033[1;35m[{timestamp_str}] {node_name.upper()} → {action}\033[0m")
        if details:
            truncated_details = details[:100] + "..." if len(details) > 100 else details
            print(f"\033[0;35m  Details: {truncated_details}\033[0m")

    # --- Node Implementations ---

    def generate_plan(self, state: AgentState):
        """Generate a detailed plan based on user request."""
        self._log_execution_step("PLAN", "Generating execution plan")

        # Extract the user request from the last human message
        human_messages = [msg for msg in state["messages"] if isinstance(msg, HumanMessage)]
        if human_messages:
            original_request = human_messages[-1].content
        else:
            original_request = "No clear request found"

        previous_plan = state["previous_plan"]
        feedback_for_plan = state["feedback_for_plan"]

        current_step_index = state["current_step_index"]
        plan_steps = state["plan_steps"][:current_step_index]
        step_results = summarize_step_results(plan_steps)

        plan_messages = [
            SystemMessage(content=PLAN_PROMPT),
            HumanMessage(content=f"Create a detailed plan for:\n{original_request}\n\nPrevious plan:\n{previous_plan}\n\nFeedback for previous plan:\n{feedback_for_plan}\n\nPrevious step results:\n{step_results}")
        ]

        response = self.planner_llm.invoke(plan_messages)
        # after you get `response` from the LLM...
        raw = response.content.strip()

        # look for ```json … ``` and pull out the inner bit
        m = re.search(r'```(?:json)?\s*(.*?)\s*```', raw, re.DOTALL)
        if m:
            payload = m.group(1)
        else:
            payload = raw

        try:
            # Parse the JSON response
            plan_data = json.loads(payload)

            for step_data in plan_data.get("steps", []):
                plan_step: PlanStep = {
                    "step_number": step_data["step_number"],
                    "description": step_data["description"],
                    "tool": step_data["tool"],
                    "estimated_time": step_data["estimated_time"],
                    "status": "pending",
                    "result": None,
                    "feedback": None
                }
                plan_steps.append(plan_step)

            # Format plan for user review
            plan_summary = plan_to_text(plan_steps, current_step_index)
            state["previous_plan"] = plan_summary

            plan_summary += "\n**Commands:**\n"
            plan_summary += "- Type `approve` to execute this plan\n"
            plan_summary += "- Type `exit` to abort\n"
            plan_summary += "- Type `[your feedback]` to request changes\n"

            return {
                "messages": [AIMessage(content=plan_summary)],
                "original_request": original_request,
                "plan_steps": plan_steps,
            }

        except json.JSONDecodeError:
            error_msg = f"Failed to parse plan: {payload}. Please try again with a clearer request."
            return {
                "messages": [AIMessage(content=error_msg)],
            }

    def wait_for_plan_review(self, state: AgentState):
        """Wait for user to review and approve the plan."""
        self._log_execution_step("WAIT_PLAN_REVIEW", "Waiting for user plan review")

        user_input = input("\n>>> ").strip()

        return {"feedback_for_plan": user_input}

    def plan_review_decision(self, state: AgentState) -> str:
        """Decide what to do based on user's plan review."""
        # Check the last human message for approval/feedback
        feedback_for_plan = state["feedback_for_plan"]
        if not feedback_for_plan:
            return "waiting"

        if feedback_for_plan == "approve":
            return "approved"
        elif feedback_for_plan == "exit":
            return "exit"
        else:
            return "needs_revision"

    def execute_current_step(self, state: AgentState):
        """Execute the current step in the plan."""
        current_step_index = state["current_step_index"]
        plan_steps = state["plan_steps"]

        if current_step_index >= len(plan_steps):
            return {}

        current_step = plan_steps[current_step_index]
        self._log_execution_step("EXECUTE_STEP", f"Executing step {current_step['step_number']}")

        # Update step status
        current_step["status"] = "in_progress"

        # Prepare execution context
        step_context = state.get("step_context", "")
        step_context += f"\nStep {current_step['step_number']}: {current_step['description']}\n"

        # Create execution prompt
        execution_prompt = STEP_EXECUTION_PROMPT.format(
            current_step=json.dumps(current_step, indent=2),
            step_context=step_context
        )

        execution_messages = [
            SystemMessage(content=execution_prompt),
            HumanMessage(content=f"Execute: {current_step['description']}")
        ]

        # Create model with tools
        model_with_tools = self.executor_llm.bind_tools(self.tools)
        response = model_with_tools.invoke(execution_messages)

        # If the response includes tool calls, execute them
        if hasattr(response, "tool_calls") and response.tool_calls:
            tool_results = []
            for tool_call in response.tool_calls:
                tool_name = tool_call["name"]
                tool_args = tool_call["args"]

                self._log_execution_step("TOOL_EXECUTION", f"Executing {tool_name}", str(tool_args))

                tool_result = self.tools_by_name[tool_name].invoke(tool_args)
                tool_results.append(f"Tool: {tool_name}\nResult: {tool_result}")

            # Combine results
            combined_results = "\n\n".join(tool_results)
            current_step["result"] = combined_results
            current_step["status"] = "completed"

        else:
            # No tool calls, just a text response
            combined_results = ""
            current_step["result"] = response.content
            current_step["status"] = "completed"

        next_step = "N/A\n"
        if current_step_index+1 < len(plan_steps):
            next_step = step_to_text(plan_steps[current_step_index+1], current_step_index+1, current_step_index+1)

        step_summary = f"## Step {current_step['step_number']} Completed\n\n"
        step_summary += f"**Description:** {current_step['description']}\n\n"
        step_summary += f"**Results:**\n```\n{current_step['result']}\n```\n\n"
        step_summary += f"**Next Step:**\n{next_step}"
        step_summary += "**Commands:**\n"
        step_summary += "- Type `continue` to proceed to next step\n"
        step_summary += "- Type `skip` to skip to next step\n"
        step_summary += "- Type `redo` to redo this step\n"
        step_summary += "- Type `[feedback]` to replan with modifications\n"

        # Update state
        updated_steps = plan_steps.copy()
        updated_steps[current_step_index] = current_step

        return {
            "messages": [AIMessage(content=step_summary)],
            "plan_steps": updated_steps,
            "step_context": step_context + f"Result: {current_step['result']}\n",
        }

    def wait_for_step_feedback(self, state: AgentState):
        """Wait for user feedback on the completed step."""
        self._log_execution_step("WAIT_STEP_FEEDBACK", "Waiting for user step feedback")
        messages = state["messages"]
        feedback_for_plan = state["feedback_for_plan"]
        user_input = input("\n>>> ").strip()
        if user_input:
            messages.append(HumanMessage(content=user_input))
            if user_input not in ["continue", "skip", "redo"]:
                feedback_for_plan = user_input

        return {"messages": messages, "feedback_for_plan": feedback_for_plan}

    def step_feedback_decision(self, state: AgentState) -> str:
        """Decide what to do based on user's step feedback."""
        human_messages = [msg for msg in state["messages"] if isinstance(msg, HumanMessage)]
        if human_messages:
            last_human_msg = human_messages[-1].content.lower().strip()

            if last_human_msg == "continue":
                # Move to next step
                new_index = state["current_step_index"] + 1
                if new_index >= len(state["plan_steps"]):
                    return "completed"
                else:
                    return "continue"
            elif last_human_msg == "skip":
                new_index = state["current_step_index"] + 1
                if new_index >= len(state["plan_steps"]):
                    return "completed"
                else:
                    return "continue"
            elif last_human_msg == "redo":
                return "redo"
            else:
                return "continue"

        return "waiting"

    def increment_step_index(self, state: AgentState):
        """Increment step index."""
        self._log_execution_step("INCREMENT_STEP_INDEX", "Incrementing step index")

        new_index = state["current_step_index"] + 1
        return { "current_step_index": new_index }

    def reflect_on_completion(self, state: AgentState):
        """Reflect on the completion of all steps."""
        self._log_execution_step("REFLECT", "Reflecting on completion")

        # Prepare reflection data
        step_results = summarize_step_results(state["plan_steps"])
        reflection_prompt = REFLECTION_PROMPT.format(
            original_request=state["original_request"],
            executed_plan=json.dumps([{k: v for k, v in step.items() if k != "result"} for step in state["plan_steps"]], indent=2),
            step_results=step_results
        )

        reflection_messages = [
            SystemMessage(content=reflection_prompt),
            HumanMessage(content="Please evaluate the completion of the plan.")
        ]

        response = self.planner_llm.invoke(reflection_messages)

        reflection_summary = f"## Task Completion Review\n\n{response.content}\n\n"
        reflection_summary += "**Commands:**\n"
        reflection_summary += "- Type `done` if satisfied with the results\n"
        reflection_summary += "- Type `replan: [feedback]` to create a new plan with modifications\n"

        return {
            "messages": [AIMessage(content=reflection_summary)],
        }

    def wait_for_final_review(self, state: AgentState):
        """Wait for user's final review."""
        self._log_execution_step("WAIT_FINAL_REVIEW", "Waiting for final user review")

        messages = state["messages"]
        user_input = input("\n>>> ").strip()
        if user_input:
            messages.append(HumanMessage(content=user_input))

        return {"messages": messages}

    def final_review_decision(self, state: AgentState) -> str:
        """Decide what to do based on user's final review."""
        human_messages = [msg for msg in state["messages"] if isinstance(msg, HumanMessage)]
        if human_messages:
            last_human_msg = human_messages[-1].content.lower().strip()

            if last_human_msg == "done":
                return "success"
            elif last_human_msg.startswith("replan:"):
                return "replan"

        return "waiting"

    # --- Public Interface ---

    def run(self, query: str, thread_id: str = None):
        """Run the enhanced agent with user feedback loops."""
        if thread_id:
            self.thread_id = thread_id
            self.config = RunnableConfig(configurable={"thread_id": thread_id})

        print(f"\n\033[1;34mStarting enhanced execution for query: {query}\033[0m")
        self.execution_log.clear()

        # Get current state
        current_state = self.graph.get_state(self.config)
        if current_state and current_state.values:
            existing_messages = current_state.values.get("messages", [])
        else:
            existing_messages = [SystemMessage(content=self.system_prompt)]

        # Add new human message
        new_messages = existing_messages + [HumanMessage(content=query)]
        initial_state = {
            "messages": new_messages,
            "original_request": query,
            "previous_plan": "N/A",
            "feedback_for_plan": "N/A",
            "plan_steps": [],
            "current_step_index": 0,
            "step_context": "",
        }

        state = initial_state
        while True:
            try:
                # Stream the execution
                for event in self.graph.stream(state, config=self.config, stream_mode="values"):
                    latest_message = event["messages"][-1]

                    # Only print AI messages
                    if isinstance(latest_message, AIMessage):
                        print(f"\n\033[1;32m{latest_message.content}\033[0m")

                else:
                    break

            except Exception as e:
                print(f"\n\033[1;31mAn unexpected error occurred: {e}\033[0m")
                traceback.print_exc()

    def clear_history(self):
        """Clear conversation history and reset state."""
        self.execution_log.clear()
        try:
            initial_state = {
                "messages": [SystemMessage(content=self.system_prompt)],
                "original_request": "",
                "previous_plan": "N/A",
                "feedback_for_plan": "N/A",
                "plan_steps": [],
                "current_step_index": 0,
                "step_context": "",
            }
            self.graph.update_state(self.config, initial_state, as_node="__start__")
            print("Cleared conversation history and reset state")
        except Exception as e:
            print(f"Could not clear state: {e}")


def main():
    """Main function to run the Enhanced Emacs Agent."""
    print("Welcome to the Enhanced Emacs Agent with User Feedback!")
    print("This agent will:")
    print("1. Create a detailed plan for your request")
    print("2. Wait for your approval before executing")
    print("3. Execute steps one by one with your feedback")
    print("4. Reflect on completion and allow refinements")
    print("\nSpecial commands:")
    print("  - 'exit' or 'quit': end the session")
    print("  - 'clear': clear conversation history and execution log")

    try:
        agent = EmacsAgent()
        while True:
            query = input("\n> ")
            if query.lower() in ["exit", "quit"]:
                break
            elif query.lower() == "clear":
                agent.clear_history()
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
