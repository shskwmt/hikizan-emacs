;;; hikizan-llm.el --- LLM configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for LLM integrations

;;; Code:

(require 'use-package)

;;;; User Configurable Variables

(defvar hikizan/ellama-chat-model "llama3.2"
  "Default chat model for Ellama.")

(defvar hikizan/ellama-embedding-model "nomic-embed-text"
  "Default embedding model for Ellama.")

(defvar hikizan/claude-api-key ""
  "Claude api key.")

;;;; Ellama setup

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
	   :chat-model hikizan/ellama-chat-model
	   :embedding-model hikizan/ellama-embedding-model)))

;;;; GPTel setup

(use-package gptel
  :ensure t
  :custom
  (gptel-model "claude-3-5-haiku-20241022")
  :config
  (setq gptel-log-level 'debug)
  (setq gptel-confirm-tool-calls t)
  (setq gptel-backend
	(gptel-make-anthropic "Claude"
	  :key (lambda ()
		 (or hikizan/claude-api-key
		     (error "Claude API key not set")))
	  :stream t))
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
