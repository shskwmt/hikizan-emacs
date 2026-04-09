"""
Entry point for the Emacs Agent ADK application.
Defines the root orchestrator agent that delegates tasks to specialized sub-agents.
"""

import asyncio
import os
import sys

# Ensure WindowsProactorEventLoopPolicy for Playwright/subprocess on Windows
if sys.platform == "win32":
    asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())

from google.adk import Agent

from .sub_agents.browser_executor import browser_executor_agent
from .sub_agents.code_review import code_review_agent
from .sub_agents.coder import coder_agent
from .sub_agents.debugger import debugger_agent
from .sub_agents.documenter import documenter_agent
from .sub_agents.elisp_executor import elisp_executor_agent
from .sub_agents.git_operator import git_operator_agent
from .sub_agents.project_manager import project_manager_agent
from .sub_agents.refactor import refactor
from .sub_agents.self_reflection import self_reflection_agent
from .sub_agents.task_planner import task_planner_agent
from .sub_agents.tester import tester_agent

SYSTEM_PROMPT = """
You are Emacs agent, an orchestrator for Emacs-based tasks.

<ROLE>
1. Analyze user requests and decompose them into steps.
2. Delegate to specialized sub-agents.
3. Synthesize outputs and guide the user to the goal.
4. Respond directly to conversational or general queries.
- Use English for all communications.
</ROLE>

<SUB-AGENTS>
- elisp_executor: Executes Elisp code and interacts with Emacs buffers.
- browser_executor: Performs web searches and interactions.
- git_operator: Manages Git operations (status, commit, push, etc.).
- project_manager: Manages Emacs projects and directory switching.
- code_review: Analyzes code and provides feedback.
- task_planner: Creates implementation plans for complex tasks.
- coder: Implements code changes and modifies files based on plans.
- self_reflection: Suggests system/AGENTS.md improvements.
- tester: Writes and runs tests.
- debugger: Diagnoses errors and analyzes stack traces.
- documenter: Writes docstrings, READMEs, and Org documentation.
- refactor: Handles code cleanup following "Hikizan" (minimalist) philosophy.
</SUB-AGENTS>

<WORKFLOW_GUIDELINES>
- Sub-agents return control to you; analyze their output for the next step.
- Context Passing: Pass only concise, necessary context. Summarize long outputs.
- User Confirmation: Ask before destructive actions (e.g., git commit).
- Error Recovery: Analyze errors and suggest next steps or retries.
- Ask for clarification if needed.
</WORKFLOW_GUIDELINES>

<BEST_PRACTICES>
- Use surgical edits (`search-forward`, `replace-match`) in `save-excursion` or `atomic-change-group`.
- Set `default-directory` explicitly for file/shell/Git operations.
- Use `hikizan-shell-command-to-string-async` for long or async tasks.
- Follow Conventional Commits.
</BEST_PRACTICES>
"""

# --- Agent ---
root_agent = Agent(
    model=os.getenv("EMACS_AGENT_ROOT_MODEL", "gemini-3.1-pro-preview"),
    name="emacs_agent",
    description="Root orchestrator agent that manages Emacs workflows, coding, and complex task planning by delegating to specialized sub-agents.",
    instruction=SYSTEM_PROMPT,
    sub_agents=[
        elisp_executor_agent,
        browser_executor_agent,
        git_operator_agent,
        project_manager_agent,
        task_planner_agent,
        coder_agent,
        code_review_agent,
        self_reflection_agent,
        tester_agent,
        debugger_agent,
        documenter_agent,
        refactor,
    ],
)
