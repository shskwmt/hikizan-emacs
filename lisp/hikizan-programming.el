;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;; ruby
(add-to-list 'auto-mode-alist '("\\.rbs\\'" . ruby-mode))

;; go
(use-package go-mode
  :ensure t)

;; yaml
(use-package yaml-mode
  :ensure t)

;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; terraform
(use-package terraform-mode
  :ensure t)

;; lsp
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (ruby-mode . lsp)
  (go-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode))

;; ein https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :ensure t)

(provide 'hikizan-programming)
