ELISP_INSTRUCTIONS = """
<InstructionsOfExecuteElispCode>
- **Output**: Always use `(message "%s" ...)` or `(print ...)` to ensure output is captured. For large outputs, prefer `message`.
- **Async Operations**: Use `hikizan-shell-command-to-string-async` for external commands like `git grep`, `git ls-files`, or long-running shell tasks.
- **File Manipulation**:
    - Use `find-file-noselect` and `with-current-buffer` for background file operations.
    - Prefer surgical edits with `search-forward`, `replace-match`, and `atomic-change-group`.
    - Always `(save-buffer)` after modifications.
    - Set `default-directory` explicitly to ensure correct context.
- **String Literals**: Double escape backslashes (e.g., `\\\\`) in Python strings for Elisp.
- **Path Handling**: Use `expand-file-name` for absolute paths and `file-equal-p` for comparisons.
- **Git Integration**: Follow Conventional Commits. Use `git status` to verify state.
- **Verification**: After any edit, verify the buffer state (e.g., search for the changed text).
</InstructionsOfExecuteElispCode>
"""
