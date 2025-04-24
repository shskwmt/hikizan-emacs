;;; hikizan-llm.el --- LLM configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive configuration for LLM integrations in hikizan-emacs

;;; Code:

(require 'use-package)

;;;; Custom Configurable Variables

(defgroup hikizan/llm nil
  "Customization group for Hikizan LLM configurations."
  :group 'tools
  :prefix "hikizan/llm-")

(defcustom hikizan/llm-ellama-chat-model "llama3.2"
  "Default chat model for Ellama."
  :type 'string
  :group 'hikizan/llm)

(defcustom hikizan/llm-ellama-embedding-model "nomic-embed-text"
  "Default embedding model for Ellama."
  :type 'string
  :group 'hikizan/llm)

(defcustom hikizan/llm-claude-api-key ""
  "API key for Claude LLM."
  :type 'string
  :group 'hikizan/llm
  :safe #'stringp)

;;;; Utility Functions

(defun hikizan/llm--get-claude-api-key ()
  "Retrieve Claude API key with error handling."
  (or hikizan/llm-claude-api-key
      (user-error "Claude API key not configured")))

(defun hikizan/llm--create-system-prompt ()
  "Generate a comprehensive system prompt for GPTel."
  (concat
   "** Role\n"
   "You are a large language model living in Emacs and helpful assistant.\n\n"
   "** Context\n"
   "- I am using emacs to automate programming tasks.\n\n"
   "** Instructions\n"
   "- Write efficient ELISP code to perform the objective.\n\n"
   "** ELISP Code Examples:\n"
   "#+begin_src emacs-lisp\n"
   ";; Retrieve a list of buffers\n"
   "(with-current-buffer (list-buffers-noselect)\n"
   " (buffer-substring-no-properties (point-min) (point-max)))\n"
   "#+end_src\n\n"
   "#+begin_src emacs-lisp\n"
   ";; Read the contents of a specific buffer by name\n"
   "(with-current-buffer \"{buffer-name}\" ;; Replace {buffer-name} with the actual buffer name\n"
   "  (buffer-substring-no-properties (point-min) (point-max)))\n"
   "#+end_src"))

;;;; Ellama Configuration

(use-package ellama
  :ensure t
  :init
  (setq ellama-keymap-prefix "C-c e")
  :custom
  (ellama-language "English")
  (ellama-auto-scroll t)
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model hikizan/llm-ellama-chat-model
           :embedding-model hikizan/llm-ellama-embedding-model)))

;;;; GPTel Configuration

(use-package gptel
  :ensure t
  :custom
  (gptel-model "claude-3-5-haiku-20241022")
  :config
  (setq gptel-log-level 'debug)
  (setq gptel-confirm-tool-calls t)
  (setq gptel-include-tool-results t)
  (setq gptel--system-message (hikizan/llm--create-system-prompt))

  ;; Claude Backend Configuration
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :key #'hikizan/llm--get-claude-api-key
          :stream t))
  
  ;; Emacs Lisp Evaluation Tool
  (setq gptel-tools
        (list
         (gptel-make-tool
          :name "eval_elisp"
          :function #'hikizan/safe-elisp-eval
          :description "Safely evaluate elisp in Emacs."
          :args '((:name "elisp"
                   :type "string"
                   :description "Elisp code to execute"))
          :category "emacs"))))

(provide 'hikizan-llm)
;;; hikizan-llm.el ends here
