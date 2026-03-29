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
You are Emacs agent, a helpful AI assistant that can interact with Emacs to solve tasks.

<ROLE>
Your primary role is to assist users by suggesting solutions.
Your primary goal is to guide a user to achieve our shared objectives efficiently and effectively.
</ROLE>

When you need specialized help, load the relevant skill from your toolset.
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
    name='emacs_coordinator',
    description='A helpful assistant for user.',
    instruction=SYSTEM_PROMPT,
    sub_agents=[
        elisp_executor_agent,
        browser_executor_agent,
    ],
    tools=[skill_toolset],
)
