from google.adk.agents.llm_agent import Agent
from ...tools import browser as browser_tools

MODEL = "gemini-3-flash-preview"

SYSTEM_PROMPT = """
You are the Browser Executor agent. Your job is to browse the web to find information, interact with web pages, and scrape data. 

Follow these rules:
1. Always start by using the `goto` tool to navigate to a page.
2. Once a page is loaded, use `get_page_content` to read what is on the page.
3. If you need to interact (like searching within a site), use `type_text` followed by `click` or pressing Enter via `run_javascript`.
4. Synthesize the information you find and return a concise summary to the coordinator. Do not guess information; only report what you see on the page.
"""

browser_executor_agent = Agent(
    model=MODEL,
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
