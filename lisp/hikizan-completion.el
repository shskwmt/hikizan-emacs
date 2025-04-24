;;; hikizan-completion.el --- completion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion settings for hikizan-emacs

;;; Code:

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

;; vertico https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; consult https://github.com/minad/consult
(use-package consult
  :ensure t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

;; marginalia https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'hikizan-completion)
;;; hikizan-completion.el ends here
