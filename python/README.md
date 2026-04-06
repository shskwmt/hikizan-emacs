# Emacs Agent

## Usage of Emacs Agent

The =emacs_agent= is a Python-based AI assistant that acts as a bridge between an external LLM process and a running Emacs instance. It functions as an **orchestrator**, breaking down complex tasks and delegating them to specialized sub-agents and skills to manipulate the Emacs environment, execute code, or perform web searches.

### Running the Agent

To start the agent session, execute the module from your terminal in the project's root directory:

```shell
# Depending on your entry point script, you might run:
adk run .
```

### Prerequisites

Before running the agent, ensure the following:

- The `GOOGLE_API_KEY` environment variable is set with your Google API key.
- Python dependencies are installed. Run `pip install .` and `playwright install` to install them.
