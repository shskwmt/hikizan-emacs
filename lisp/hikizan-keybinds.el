;;; hikizan-keybinds.el --- keybinds -*- lexical-binding: t; -*-

;;; global key bindings

;; hikizan toggle
(global-set-key (kbd "C-c b") 'hikizan/toggle-buffer-list-window)
(global-set-key (kbd "C-c s") 'hikizan/toggle-scratch-window)
(global-set-key (kbd "C-c o") 'hikizan/toggle-org-note-window)
(global-set-key (kbd "C-c m") 'hikizan/toggle-messages-window)

;; others
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)
(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "M-w") 'kill-ring-save)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

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
