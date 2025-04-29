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

;; embark https://github.com/oantolin/embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'hikizan-completion)
;;; hikizan-completion.el ends here
