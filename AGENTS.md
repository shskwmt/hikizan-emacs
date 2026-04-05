# Hikizan Emacs - Project Guidelines for AI Agents

Welcome to **hikizan emacs**, a minimal and modular Emacs configuration environment. 
When operating in this project, adhere to the following principles and structure.

## 1. Project Philosophy
- **Minimalism (Hikizan)**: Keep configurations simple and avoid adding heavy, unnecessary dependencies. "Hikizan" implies subtraction.
- **Modularity**: Configurations are broken down into specific topics (e.g., UI, programming, org-mode) inside the `lisp/` directory.

## 2. Directory Structure
- `init.el`: The main entry point that requires modules from the `lisp/` directory. Do not clutter this file with configuration logic.
- `early-init.el`: Used strictly for very early initialization, such as loading environment variables.
- `lisp/`: Contains custom Emacs Lisp configuration modules.
- `python/`: Contains the Python codebase for the AI agents (`emacs_agent`, etc.) operating in this system.
- `~/.emacsenv`: Used by the user to set specific environment variables (loaded in `early-init.el`).

## 3. Emacs Lisp Coding Conventions
- **Lexical Binding**: Always use `;;; -*- lexical-binding: t; -*-` at the top of any new `.el` file.
- **Naming Conventions**: 
  - All custom files in `lisp/` must be prefixed with `hikizan-` (e.g., `hikizan-feature.el`).
  - All variables, functions, and custom groups defined in these files must use the `hikizan-` prefix.
- **Provide/Require**: End each custom `.el` file with `(provide 'hikizan-feature)` and require it in `init.el` or where appropriate.

## 4. Python Coding Conventions
- When making changes in the `python/` directory, ensure compatibility with the existing agent architecture.
- Maintain clear separation between the orchestrator (`emacs_agent`) and sub-agents.

## 5. AI Sub-Agents & Roles
When collaborating, use the following agents for their specialized tasks:
- **elisp_executor**: Direct Emacs Lisp execution and buffer manipulation.
- **browser_executor**: Web searching and internet interaction.
- **git_operator**: Project-wide Git operations.
- **project_manager**: Directory switching and project discovery.
- **coder / code_review**: Implementation and feedback for code changes.
- **task_planner**: Architectural planning for complex features.
- **self_reflection**: Post-task analysis to improve these very definitions and workflow guidelines.

## 6. AI Agent Operational Best Practices
- **Path Consistency**: Always verify and set the `default-directory` explicitly when performing file, shell, or Git operations. Do not assume the current environment is already at the project root.
- **Targeted Edits**: When modifying existing code via `execute_elisp_code`, use surgical edits (`search-forward`, `replace-match`, `delete-region`) rather than overwriting the entire buffer. This minimizes character escaping errors and prevents accidental overwrites of unrelated code.
- **Error Recovery**: If an Elisp command fails due to quoting or escaping issues, simplify the command or use `buffer-string` to inspect the state before retrying.
- **Conventional Commits**: All project changes should be committed using Conventional Commits (e.g., `feat:`, `fix:`, `refactor:`) to maintain a clear history.

