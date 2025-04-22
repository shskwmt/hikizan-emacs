;;; hikizan-keybinds.el --- Keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Key bindings and which-key setup for hikizan configuration.

;; Code:

;;;; Keybindings

;; Toggle windows
(global-set-key (kbd "C-c b") 'hikizan/toggle-buffer-list-window)
(global-set-key (kbd "C-c s") 'hikizan/toggle-scratch-window)
(global-set-key (kbd "C-c o") 'hikizan/toggle-org-note-window)
(global-set-key (kbd "C-c m") 'hikizan/toggle-messages-window)

;; Scrolling
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-other-window 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-other-window-down 1)))

;; Text scale
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)

;; Navigation
(global-set-key (kbd "C-;") 'avy-goto-char)

;; Org-mode
(global-set-key (kbd "C-c n") 'hikizan/open-org-notes)

;; LLM tools
(global-set-key (kbd "C-c g") 'gptel)
(global-set-key (kbd "C-c t") 'gptel-tool)

;;; which-key Configuration

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

;;; hikizan-keybinds.el ends here
