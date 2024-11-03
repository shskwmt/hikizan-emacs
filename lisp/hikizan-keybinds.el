;;; hikizan-keybinds.el --- keybinds -*- lexical-binding: t; -*-

;;; global key bindings
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)
(global-set-key (kbd "C-;") 'avy-goto-char)

;;; which-key

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot 10
	which-key-idle-delay 0.1)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(provide 'hikizan-keybinds)
