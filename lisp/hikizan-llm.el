;;; hikizan-llm.el --- llm  -*- lexical-binding: t; -*-

;; ellama
(unless (boundp 'ellama-chat-model)
  (setq ellama-chat-model "llama3.2"))

(unless (boundp 'ellama-embedding-model)
  (setq ellama-embedding-model "nomic-embed-text"))

(use-package ellama
  :ensure t
  :init
  (setq ellama-keymap-prefix "C-c e")
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model ellama-chat-model :embedding-model ellama-embedding-model)))

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
