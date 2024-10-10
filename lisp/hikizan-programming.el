;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;;; ruby
(add-to-list 'auto-mode-alist '("\\.rbs\\'" . ruby-mode))

;;; go
(use-package go-mode
  :ensure t)

;;; terraform
(use-package terraform-mode
  :ensure t)

(provide 'hikizan-programming)
