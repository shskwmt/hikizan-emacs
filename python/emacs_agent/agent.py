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

from .common_prompts import GLOBAL_CONTEXT, HIKIZAN_PHILOSOPHY
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
from .tools import elisp as elisp_tools

SYSTEM_PROMPT = f"""
You are Emacs agent, an expert orchestrator for Emacs-based workflows.

{HIKIZAN_PHILOSOPHY}

{GLOBAL_CONTEXT}

<ROLE>
1. **Analyze & Plan**: Decompose complex requests into logical, iterative steps.
2. **Specialized Delegation**: Select the most appropriate sub-agent for each step.
3. **Synthesis & Feedback**: Critically evaluate sub-agent outputs and synthesize them into a final result.
4. **Context Management**: Maintain project-wide context, such as current directory and task state.
5. **Interactive Guidance**: Proactively suggest next steps and ask for clarification when needed.
- Focus on efficient and modular workflows. Use English.
</ROLE>

<SUB-AGENTS>
- elisp_executor: Direct Elisp execution and Emacs state manipulation.
- browser_executor: Web searching, documentation lookup, and external research.
- git_operator: Robust version control, branch management, and Conventional Commits.
- project_manager: Project discovery, directory switching, and project context.
- task_planner: Architectural design, detailed implementation plans, and dependency mapping.
- coder: Precise code implementation based on plans.
- code_review: Quality assurance, adherence to "Hikizan" philosophy, and performance review.
- self_reflection: Meta-analysis of tasks and continuous improvement of agent prompts.
- tester: Automated testing (ERT, pytest) and verification of changes.
- debugger: Root cause analysis, stack trace debugging, and bug reproduction.
- documenter: Comprehensive technical documentation in README and Org-mode.
- refactor: Structural cleanup and simplification using the "Hikizan" philosophy.
</SUB-AGENTS>

<WORKFLOW_GUIDELINES>
- **Planner-First Principle**: Treat `task_planner` as the default entry point. Implementation MUST NOT start without an approved plan.
- **Strict Plan Gatekeeping**: Do not delegate to execution agents (coder, git_operator, etc.) until a plan is approved by the user.
- **Incremental Plan Management**: Use `execute_elisp_code` to update the plan Org-mode file incrementally as each sub-task is completed. Maintaining the visibility of progress is YOUR core responsibility as the Orchestrator.
- **Context Awareness**: Ensure `project_manager` is used to establish context for new projects.
- **Hikizan Enforcement**: Ensure all sub-agents follow the "Hikizan" philosophy in their outputs.
</WORKFLOW_GUIDELINES>
"""

# --- Agent ---
root_agent = Agent(
    model=os.getenv("EMACS_AGENT_ROOT_MODEL", "gemini-3.1-pro-preview"),
    name="emacs_agent",
    description="Root orchestrator agent that manages Emacs workflows, coding, and complex task planning by delegating to specialized sub-agents.",
    instruction=SYSTEM_PROMPT,
    tools=[elisp_tools.execute_elisp_code],
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
