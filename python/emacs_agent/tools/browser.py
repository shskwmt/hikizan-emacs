import json
import queue
import threading

from playwright.sync_api import sync_playwright


class BrowserThread(threading.Thread):
    _instance = None
    _lock = threading.Lock()

    def __init__(self):
        super().__init__(name="BrowserThread", daemon=True)
        self.cmd_queue = queue.Queue()
        self.headless = True
        self.start()

    @classmethod
    def get_instance(cls):
        with cls._lock:
            if cls._instance is None:
                cls._instance = cls()
            return cls._instance

    def run(self):
        try:
            with sync_playwright() as p:
                browser = p.chromium.launch(headless=self.headless)
                context = browser.new_context(
                    viewport={"width": 1280, "height": 800},
                    user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36",
                )
                page = context.new_page()

                while True:
                    func, args, kwargs, result_queue = self.cmd_queue.get()
                    if func is None:  # Shutdown signal
                        break
                    try:
                        res = func(page, *args, **kwargs)
                        result_queue.put((res, None))
                    except Exception as e:
                        result_queue.put((None, e))
                browser.close()
        except Exception as fatal_e:
            print(f"Fatal error in BrowserThread: {fatal_e}")

    def execute(self, func, *args, **kwargs):
        result_queue = queue.Queue()
        self.cmd_queue.put((func, args, kwargs, result_queue))
        res, err = result_queue.get()
        if err:
            raise err
        return res


def _goto_impl(page, url):
    if not url.startswith("http"):
        url = "https://" + url
    page.goto(url, wait_until="networkidle")
    return f"Success: Navigated to {page.title()} ({page.url})"


def goto(url: str) -> str:
    """Navigates the browser to a specific URL."""
    try:
        return BrowserThread.get_instance().execute(_goto_impl, url)
    except Exception as e:
        return f"Error navigating: {str(e)}"


def _get_page_content_impl(page):
    content = page.evaluate("document.body.innerText")
    return content[:15000]


def get_page_content() -> str:
    """Extracts and returns the visible text content of the current webpage."""
    try:
        return BrowserThread.get_instance().execute(_get_page_content_impl)
    except Exception as e:
        return f"Error extracting content: {str(e)}"


def _click_impl(page, selector):
    page.click(selector, timeout=5000)
    page.wait_for_load_state("networkidle")
    return f"Success: Clicked '{selector}'. Current URL is {page.url}"


def click(selector: str) -> str:
    """Clicks on an element on the current page using a CSS selector."""
    try:
        return BrowserThread.get_instance().execute(_click_impl, selector)
    except Exception as e:
        return f"Error clicking '{selector}': {str(e)}"


def _type_text_impl(page, selector, text):
    page.fill(selector, text, timeout=5000)
    return f"Success: Typed text into '{selector}'"


def type_text(selector: str, text: str) -> str:
    """Fills an input field on the page with the specified text."""
    try:
        return BrowserThread.get_instance().execute(_type_text_impl, selector, text)
    except Exception as e:
        return f"Error typing into '{selector}': {str(e)}"


def _run_javascript_impl(page, script):
    result = page.evaluate(script)
    return f"JS Result: {json.dumps(result)}"


def run_javascript(script: str) -> str:
    """Executes arbitrary JS in the console."""
    try:
        return BrowserThread.get_instance().execute(_run_javascript_impl, script)
    except Exception as e:
        return f"Error executing JS: {str(e)}"
