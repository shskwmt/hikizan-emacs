# Hikizan Emacs - Project Guidelines for AI Agents

Welcome to **hikizan emacs**, a minimal and modular Emacs configuration environment. 
When operating in this project, adhere to the following principles and structure.

## 1. Project Philosophy
- **Minimalism (Hikizan)**: Keep configurations simple and avoid adding heavy, unnecessary dependencies. "Hikizan" implies subtraction.
- **Modularity**: Configurations are broken down into specific topics (e.g., UI, programming, org-mode) inside the `lisp/` directory.
- **Context from Structure**: Prefer generic filenames (e.g., `plan.org`, `session.json`) over prefixed ones when files are already isolated within scoped directories (e.g., `sessions/<id>/`).

## 2. Directory Structure
- `init.el`: The main entry point that requires modules from the `lisp/` directory. Do not clutter this file with configuration logic.
- `early-init.el`: Used strictly for very early initialization, such as loading environment variables.
- `lisp/`: Contains custom Emacs Lisp configuration modules.
- `python/`: Contains the Python codebase for the AI agents (`emacs_agent`, etc.) operating in this system.
- `~/.emacsenv`: Used by the user to set specific environment variables (loaded in `early-init.el`).
- `python/emacs_agent/sessions/`: (Internal) Default base directory for session storage (`EMACS_AGENT_SESSIONS_BASE_DIR`). Stores agent session data and plans grouped by session ID. Ignored by Git.
- `python/emacs_agent/main.py`: Custom entry point for the agent to ensure robust session saving.

## 3. Emacs Lisp Coding Conventions
- **Lexical Binding**: Always use `;;; -*- lexical-binding: t; -*-` at the top of any new `.el` file.
- **Naming Conventions**: 
  - All custom files in `lisp/` must be prefixed with `hikizan-` (e.g., `hikizan-feature.el`).
  - All variables, functions, and custom groups defined in these files must use the `hikizan-` prefix.
- **Provide/Require**: End each custom `.el` file with `(provide 'hikizan-feature)` and require it in `init.el` or where appropriate.
- **Quality Assurance**: Always verify changes with `M-x byte-compile-file` to ensure no warnings (missing dependencies, forward references, or missing docstrings) remain.

## 4. Python Coding Conventions
- **Cross-Layer Impact Analysis**: When modifying data structures, storage paths, or file formats, explicitly check both Python and Emacs Lisp codebases for dependencies or UI display logic that may be affected.
- **Code Quality**: After modifying Python code, always run `ruff format .` and `ruff check . --fix` from the `python/` directory.
- **Modularity**: Maintain clear separation between the orchestrator (`emacs_agent`) and sub-agents.

## 5. AI Sub-Agents & Roles
When collaborating, use the following agents for their specialized tasks:
- **elisp_executor**: Direct Emacs Lisp execution and buffer manipulation.
- **browser_executor**: Web searching and internet interaction.
- **git_operator**: Project-wide Git operations.
- **project_manager**: Directory switching and project discovery.
- **coder / code_review**: Implementation and feedback for code changes.
- **task_planner**: Architectural planning for complex features.
- **self_reflection**: Post-task analysis to improve these very definitions and workflow guidelines.
- **tester**: Writing, running, and analyzing unit and integration tests.
- **debugger**: Diagnosing errors and isolating bugs.
- **documenter**: Writing and updating docstrings, README files, and Org-mode documentation.
- **refactor**: Code cleanup and maintaining structural integrity following the "Hikizan" philosophy.

## 6. Environment Variables
- `EMACS_AGENT_SESSIONS_BASE_DIR`: Base directory for storing session folders.
- `SESSION_ID`: The unique identifier for the current agent session.
- `EMACS_SERVER_FILE`: Path to the Emacs server socket used by tools.
