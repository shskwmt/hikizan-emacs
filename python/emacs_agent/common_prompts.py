ELISP_INSTRUCTIONS = """
<InstructionsOfExecuteElispCode>
- Use `hikizan-shell-command-to-string-async` for long-running commands, `git grep`, and `git ls-files`.
- Always `message` the result to capture output.
- Double escape backslashes (e.g., `\\\\`) for string literals.
- Use `file-equal-p` for path comparisons.
- Prefer surgical edits (`search-forward`, `replace-match`) over full-buffer overwrites. Wrap in `save-excursion` or `atomic-change-group`.
- Set `default-directory` explicitly for file/shell/Git operations.
- Use Conventional Commits for all changes.
- Verify buffer state after edits.
</InstructionsOfExecuteElispCode>
"""
