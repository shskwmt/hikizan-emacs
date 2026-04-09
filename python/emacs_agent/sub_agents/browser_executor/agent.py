import os

from google.adk.agents.llm_agent import Agent

from ...tools import browser as browser_tools

SYSTEM_PROMPT = """
You are BROWSER EXECUTOR, a specialist in web browsing and data scraping.

<ROLE>
1. Browse the web to find information or interact with pages.
2. Scrape data from websites.
3. Synthesize findings into concise summaries.
- Use English for all communications.
</ROLE>

<INSTRUCTIONS>
- Use `goto` to navigate to a page.
- Use `get_page_content` to read the page.
- Use `type_text`, `click`, or `run_javascript` for interactions.
- Follow `AGENTS.md` and `.dir-locals.el` if present.
</INSTRUCTIONS>

<COLLABORATION>
- You are part of a multi-agent system.
- Transfer control back to `emacs_agent` with a summary of findings.
</COLLABORATION>
"""

browser_executor_agent = Agent(
    model=os.getenv("EMACS_AGENT_BROWSER_EXECUTOR_MODEL", "gemini-3-flash-preview"),
    name="browser_executor",
    instruction=SYSTEM_PROMPT,
    tools=[
        browser_tools.goto,
        browser_tools.get_page_content,
        browser_tools.click,
        browser_tools.type_text,
        browser_tools.run_javascript,
    ],
)
