---
name: code-review
description: Reviews staged git changes or specific files for code quality, bugs, and best practices.
---

# Skill: Code Review

## Description
This skill analyzes uncommitted changes or specific files and provides a structured code review. It looks for potential bugs, performance issues, readability improvements, and adherence to best practices. It can also inspect the full content of changed files when the diff context is insufficient.

## Instructions
1. **Gather Context**: Use `execute_elisp_code` to retrieve the current git diff:
   - Run `(shell-command-to-string "git diff --cached")` to review staged changes.
   - If empty, run `(shell-command-to-string "git diff")` to review unstaged changes.
2. **Inspect Full Files (If Needed)**: If the git diff does not provide enough context to fully understand the changes (e.g., missing imports, class definitions, or surrounding logic), retrieve the entire content of the relevant files using `execute_elisp_code`. 
   - Example: `(with-temp-buffer (insert-file-contents "path/to/file") (buffer-string))`
3. **Analyze**: Review the diff and file contents thoroughly. Look for:
   - Logic errors or potential bugs.
   - Naming conventions and readability.
   - Performance optimizations.
   - Missing error handling or edge cases.
4. **Report**: Provide a structured review to the user. Organize your feedback into categories:
   - **Critical Issues** (Bugs, security flaws)
   - **Suggestions** (Readability, performance)
   - **Nitpicks** (Minor formatting, typos)
   - **Praise** (Call out particularly good solutions)
5. **Offer Fixes**: If applicable, offer Elisp code snippets or shell commands that the user can run to apply your suggestions.
