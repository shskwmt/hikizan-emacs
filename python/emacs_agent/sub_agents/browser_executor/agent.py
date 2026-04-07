import os

from google.adk.agents.llm_agent import Agent

from ...tools import browser as browser_tools

SYSTEM_PROMPT = """
You are the Browser Executor agent. Your job is to browse the web to find information, interact with web pages, and scrape data.

Follow these rules:
1. Always use English for all your communications.
2. Always start by using the `goto` tool to navigate to a page.
3. Once a page is loaded, use `get_page_content` to read what is on the page.
4. If you need to interact (like searching within a site), use `type_text` followed by `click` or pressing Enter via `run_javascript`.
5. Once your task is finished, use the `transfer_to_agent` tool to transfer control back to `emacs_agent`. Synthesize the information you find and provide a concise summary.

**Important**: If the context provided by `emacs_agent` includes content from an `AGENTS.md` file or a `.dir-locals.el` file, you MUST follow the instructions and project roles defined in those files as they supplement or override your default instructions.
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
