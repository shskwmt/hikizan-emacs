;;; hikizan-llm.el --- llm  -*- lexical-binding: t; -*-

;; ellama
(unless (boundp 'ellama-chat-model)
  (setq ellama-chat-model "llama3.1"))

(unless (boundp 'ellama-embedding-model)
  (setq ellama-embedding-model "llama3.1"))

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model ellama-chat-model :embedding-model ellama-embedding-model))
  (setopt ellama-provider-llama3_1 (make-llm-ollama
				    :chat-model "llama3.1"
				    :embedding-model "llama3.1"))
  (setopt ellama-provider-gemma2 (make-llm-ollama
				  :chat-model "gemma2"
				  :embedding-model "gemma2")))

(setq ellama-generate-commit-message-template
      "Personoa:

You are a software engineering expert.

Objective:

Your objective is to generate a commit message.
This message makes easy to understnd the changes of the code.

Instructions:

1. Understand what the changes do.
2. Consider the perspective of the coder making these changes.
3. Generate a commit message that clearly conveys the purpose and impact of the changes.

Example of the commit messages:

Feature Implementation
#+begin_src
feat: Add [specific feature]

- Introduced [specific feature] to [affected modules or components].
- [Optional: Briefly explain the purpose or benefits of the feature.]
#+end_src

Bug Fix
#+begin_src
fix: Resolve [specific issue]

- Fixed [specific issue] in [affected modules or components].
- [Optional: Briefly explain how the issue was fixed or the impact of the fix.]
#+end_src

Refactoring Code
#+begin_src
refactor: Improve [specific code or module]

- Refactored [specific code or module] to [reason for refactoring].
- [Optional: Briefly explain the improvements or changes made.]
#+end_src

Others
#+begin_src
chore: [describe non-functional update]

- [Optional: Briefly explain the change or update.]
#+end_src

Diff:

%s")

(defun hikizan/llm-generate-commit-message ()
  "Generate a commit message based on the result of 'git diff --cached'"
  (interactive)
  (ellama-instant (format ellama-generate-commit-message-template
			  (shell-command-to-string "git diff --cached"))))

(setq hikizan/llm-explain-code-prompt
      "Personoa:

You are a software engineering expert.

Objective:

Your objective is to explain this code to me.
I'm looking to understand its purpose, how it works, and any important details or concepts it demonstrates.

Instructions:

1. Understand the overall functionality of the code.
2. Analyze each function or variable.
3. Explain the code to me in detail.

Code:

%s")

(defun hikizan/llm-explain-code-internal (&optional provider)
  (let ((content (hikizan/extract-buffer-or-active-region-string))
	(prov (if provider
		  provider
		ellama-provider)))
    (ellama-instant (format hikizan/llm-explain-code-prompt content)
			:provider prov)))

(defun hikizan/llm-explain-code ()
  "Explain the source code of this buffer."
  (interactive)
  (hikizan/llm-explain-code-internal ellama-provider-llama3_1))

(defun hikizan/llm-explain-code-detail ()
  "Explain the source code of this buffer."
  (interactive)
  (let ((content (hikizan/extract-buffer-or-active-region-string)))
    (ellama-instant (format hikizan/llm-explain-code-prompt content)
		    :provider ellama-provider-gemma2)))

(setq hikizan/llm-review-english-prompt
      "Persona:

You are an English teacher.

Objective:

Your objectives is the following;
1. Review my English sentences
2. Translate my English sentences to Japanese

My English sentences:

```
%s
``'")

(defun hikizan/llm-review-english ()
  "Review English sentences."
  (interactive)
  (let ((content (hikizan/extract-buffer-or-active-region-string)))
    (message (format hikizan/llm-review-english-prompt content))
    (ellama-instant (format hikizan/llm-review-english-prompt content)
		    :provider ellama-provider-llama3_1)))

(provide 'hikizan-llm)
