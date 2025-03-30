;;; hikizan-llm.el --- llm  -*- lexical-binding: t; -*-

;; ellama
(unless (boundp 'ellama-chat-model)
  (setq ellama-chat-model "llama3.2"))

(unless (boundp 'ellama-embedding-model)
  (setq ellama-embedding-model "nomic-embed-text"))

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model ellama-chat-model :embedding-model ellama-embedding-model)))

(provide 'hikizan-llm)
