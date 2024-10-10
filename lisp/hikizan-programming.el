;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;;; ruby
(add-to-list 'auto-mode-alist '("\\.rbs\\'" . ruby-mode))

;;; go
(use-package go-mode
  :ensure t)

;;; terraform
(use-package terraform-mode
  :ensure t)

;;; ellama
(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "llama3.2" :embedding-model "llama3.2")))

(provide 'hikizan-programming)
