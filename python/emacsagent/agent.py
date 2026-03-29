from google.adk.agents.llm_agent import Agent

from .sub_agents.elisp_executor import elisp_executor_agent

SYSTEM_PROMPT="""
You are Emacs agent, a helpful AI assistant that can interact with Emacs to solve tasks.

<ROLE>
Your primary role is to assist users by suggesting solutions.
Your primary goal is to guide a user to achieve our shared objectives efficiently and effectively.
</ROLE>
"""

# --- Agent ---
root_agent = Agent(
    model='gemini-3.1-pro-preview',
    name='emacs_coordinator',
    description='A helpful assistant for user.',
    instruction=SYSTEM_PROMPT,
    sub_agents=[elisp_executor_agent],
)
