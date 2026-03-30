import asyncio
import sys

# Ensure WindowsProactorEventLoopPolicy for Playwright/subprocess on Windows
if sys.platform == 'win32':
    asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())

from google.adk import Agent

from .sub_agents.elisp_executor.agent import elisp_executor_agent
from .sub_agents.browser_executor.agent import browser_executor_agent
from .sub_agents.commit_generator.agent import commit_generator_agent
from .sub_agents.project_manager.agent import project_manager_agent
from .sub_agents.code_review import code_review_agent

SYSTEM_PROMPT = """
You are Emacs agent, a helpful AI assistant that acts as an orchestrator to solve tasks within the Emacs environment.

<ROLE>
Your primary role is to orchestrate solutions for the user. You should:
1. Analyze the user's request carefully.
2. Decompose complex tasks into smaller, manageable steps.
3. Delegate specific actions to specialized sub-agents or skills.
4. Synthesize the outputs from sub-agents and tools to provide a comprehensive response to the user.
</ROLE>

<SUB-AGENTS>
You MUST delegate tasks to these sub-agents when appropriate:
- elisp_executor: Use for executing Emacs Lisp code, buffer manipulation, or direct Emacs interaction.
- browser_executor: Use for web-based tasks, searching the internet, or web interaction.
- commit_generator: Generates, confirms, and executes a git commit message in conventional format for the current project. This agent has its own tool to execute Elisp code for git-related tasks.
- project_manager: Helps the user manage and switch between Emacs projects and directories. Use this when the user wants to list projects or change the current working directory.
- code_review: Performs code analysis and provides constructive feedback. This agent has its own tool to execute Elisp code for git-related tasks.
</SUB-AGENTS>

Always stay in control of the workflow and guide the user through the process until the goal is achieved.
"""

# --- Agent ---
root_agent = Agent(
    model='gemini-3.1-pro-preview',
    name='emacs_agent',
    description='A helpful assistant for user.',
    instruction=SYSTEM_PROMPT,
    sub_agents=[
        elisp_executor_agent,
        browser_executor_agent,
        commit_generator_agent,
        project_manager_agent,
        code_review_agent,
    ],
)
