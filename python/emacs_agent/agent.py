import pathlib
import asyncio
import sys

# Ensure WindowsProactorEventLoopPolicy for Playwright/subprocess on Windows
if sys.platform == 'win32':
    asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())

from google.adk import Agent
from google.adk.skills import load_skill_from_dir
from google.adk.tools.skill_toolset import SkillToolset

from .sub_agents.elisp_executor.agent import elisp_executor_agent
from .sub_agents.browser_executor.agent import browser_executor_agent

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
</SUB-AGENTS>

<SKILLS>
Use these skills to perform specialized high-level tasks:
- commit-generator: Generates commit messages based on diffs/changes.
- code-review: Performs code analysis and provides constructive feedback.
</SKILLS>

Always stay in control of the workflow and guide the user through the process until the goal is achieved.
"""

# 1. Load File-Based Skills
# Note: Directory name 'commit_generator' must match skill name in SKILL.md
commit_generator_skill = load_skill_from_dir(
    pathlib.Path(__file__).parent / "skills" / "commit-generator"
)

code_review_skill = load_skill_from_dir(
    pathlib.Path(__file__).parent / "skills" / "code-review"
)

# 2. Create SkillToolset
skill_toolset = SkillToolset(
    skills=[
        commit_generator_skill,
        code_review_skill,
    ]
)

# --- Agent ---
root_agent = Agent(
    model='gemini-3.1-pro-preview',
    name='emacs_agent',
    description='A helpful assistant for user.',
    instruction=SYSTEM_PROMPT,
    sub_agents=[
        elisp_executor_agent,
        browser_executor_agent,
    ],
    tools=[skill_toolset],
)
