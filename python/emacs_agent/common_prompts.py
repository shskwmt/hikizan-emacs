ELISP_INSTRUCTIONS = """
<InstructionsOfExecuteElispCode>
- **Output**: Always use `(message "%s" ...)` or `(print ...)` to ensure output is captured. For large outputs, prefer `message`.
- **Async Operations**: Use `hikizan-shell-command-to-string-async` for external commands like `git grep`, `git ls-files`, or long-running shell tasks to avoid blocking.
- **File Manipulation**:
    - Use `find-file-noselect` and `with-current-buffer` for background file operations. Avoid `find-file` which opens buffers in the foreground.
    - Prefer surgical edits with `search-forward`, `replace-match`, and `atomic-change-group`.
    - Always `(save-buffer)` after modifications.
    - Explicitly set `default-directory` or use absolute paths via `expand-file-name` to ensure correct context.
- **String Literals**: Double escape backslashes (e.g., `\\\\`) in Python strings for Elisp to ensure they are received correctly. To avoid `Symbol's value as variable is void: n` errors, ensure backslashes meant for Elisp are properly doubled in Python strings.
- **Multi-line Content**: For multi-line file content (like `.org` files), prefer `with-temp-buffer`, `insert`, and `write-region` over complex `search-forward`/`replace-match` chains when rewriting large blocks.
- **Path Handling**: Use `expand-file-name` for absolute paths and `file-equal-p` for comparisons.
- **Git Integration**: Follow Conventional Commits. Use `git status` to verify state.
- **Verification**: After any edit, verify the buffer state (e.g., search for the changed text) and ensure syntax is correct.
</InstructionsOfExecuteElispCode>
"""

HIKIZAN_PHILOSOPHY = """
<HIKIZAN_PHILOSOPHY>
- **Hikizan** (subtraction) philosophy: remove redundancy and simplify complex logic.
- Prefer built-in Emacs features over external packages or complex custom Lisp.
- Aim for the most concise and maintainable solution.
- Before adding code, ask: "Can this be done with less?"
</HIKIZAN_PHILOSOPHY>
"""

GLOBAL_CONTEXT = """
<GLOBAL_CONTEXT>
- Project standards are defined in `AGENTS.md` (located in the project root).
- Local environment settings may be found in `.dir-locals.el`.
- Always respect the `default-directory` and use absolute paths for file operations.
</GLOBAL_CONTEXT>
"""
