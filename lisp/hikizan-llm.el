;;; hikizan-llm.el --- llm  -*- lexical-binding: t; -*-

;; Package management and dependencies
(require 'use-package)

;; LLM Configuration Customization

;; ellama
(unless (boundp 'hikizan/ellama-chat-model)
  (setq hikizan/ellama-chat-model "llama3.2"))

(unless (boundp 'hikizan/ellama-embedding-model)
  (setq hikizan/ellama-embedding-model "nomic-embed-text"))

(use-package ellama
  :ensure t
  :custom
  (setq ellama-keymap-prefix "C-c e")
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model hikizan/ellama-chat-model
	   :embedding-model hikizan/ellama-embedding-model)))

;; GPTel Configuration
(use-package gptel
  :ensure t
  :custom
  (gptel-model "claude-3-5-haiku-20241022")
  :config
  ;; Centralized backend configuration
  (setq gptel-backend
	(gptel-make-anthropic "Claude"
	  :key (lambda ()
		 (or hikizan/claude-api-key
		     (error "Claude API key not set")))
	  :stream t))

  ;; Set gptel-tools
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

;; Provide the package
(provide 'hikizan-llm)
;;; hikizan-llm.el ends here
