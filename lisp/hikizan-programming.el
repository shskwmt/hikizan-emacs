;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;;; ruby
(add-to-list 'auto-mode-alist '("\\.rbs\\'" . ruby-mode))

;;; go
(use-package go-mode
  :ensure t)

;;; terraform
(use-package terraform-mode
  :ensure t)

;;; ein https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :ensure t)

;;; ellama
(unless (boundp 'ellama-chat-model)
  (setq ellama-chat-model "llama3"))

(unless (boundp 'ellama-embedding-model)
  (setq ellama-embedding-model "llama3"))

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model ellama-chat-model :embedding-model ellama-embedding-model)))

(provide 'hikizan-programming)
