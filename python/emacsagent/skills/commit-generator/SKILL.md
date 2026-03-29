---
name: commit-generator
description: Generates, confirms, and executes a git commit message in conventional format for the current project.
---

# Skill: Generate and Execute Git Commit

## Description
This skill identifies the current project, analyzes its git changes, proposes a high-quality **conventional commit message with a mandatory body**, and executes the commit after explicit user confirmation.

## Instructions
1.  **Identify Current Project**: Use `execute_elisp_code` to determine the project context.
    - Check the current project root:
      `(message "%s" (or (and (featurep 'project) (project-current) (project-root (project-current))) default-directory))`
    - Inform the user which project or directory you are working in.
2.  **Check Git Status**: Use `execute_elisp_code` to check the status of the repository.
    - Run: `(shell-command-to-string "git status --short")`
    - Inform the user of the current status (e.g., staged, unstaged, and untracked files).
3.  **Retrieve Git Diff**: Use `execute_elisp_code` to get the changes in that directory.
    - Check staged changes first:
      `(shell-command-to-string "git diff --cached")`
    - If empty, check unstaged changes:
      `(shell-command-to-string "git diff")`
4.  **Verify Changes**: If both diffs are empty, inform the user there are no changes to commit in the current project and stop.
5.  **Analyze and Propose**: Create a commit message:
    - **Format**: `type(scope): description`
    - **Style**: Use the imperative mood (e.g., "Add", "Fix", "Update").
    - **Length**: Keep the subject line under 50 characters.
    - **Body**: If the change is significant, include a body that explains the 'what' and 'why' of the change. Use the imperative mood and separate it from the subject line with a blank line.
6.  **Confirm with User**: Present the proposed message and ask: "Would you like to commit these changes to [Project Name/Path] with this message? (You can also provide a different message if you'd like.)"
7.  **Execute Commit**: 
    - If the user approves, use `execute_elisp_code` to run the commit command.
    - For staged changes: `(shell-command-to-string "git commit -m \"<message>\"")`
    - For unstaged changes (where you want to commit all trackable changes): `(shell-command-to-string "git commit -am \"<message>\"")`
    - **Important**: Escape any double quotes in the message if using `git commit -m "..."`.
    - Report the result of the commit to the user.
