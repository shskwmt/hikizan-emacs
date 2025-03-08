;;; hikizan-completion.el --- completion  -*- lexical-binding: t; -*-

;;; packages

;; savehist
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; orderless https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'hikizan-completion)
