;;; hikizan-keybinds.el --- keybinds -*- lexical-binding: t; -*-

;;; global key bindings

;; hikizan toggle
(global-set-key (kbd "C-c b") 'hikizan/toggle-buffer-list-window)
(global-set-key (kbd "C-c s") 'hikizan/toggle-scratch-window)
(global-set-key (kbd "C-c o") 'hikizan/toggle-org-note-window)
(global-set-key (kbd "C-c m") 'hikizan/toggle-messages-window)

;; hikizan llm
(global-set-key (kbd "C-c La") 'hikizan/llm-ask-about)
(global-set-key (kbd "C-c Lb") 'hikizan/llm-categorize-buffer-list)
(global-set-key (kbd "C-c Lec") 'hikizan/llm-explain-code)
(global-set-key (kbd "C-c Lgc") 'hikizan/llm-generate-commit-message)
(global-set-key (kbd "C-c Lgl") 'hikizan/llm-generate-elisp)
(global-set-key (kbd "C-c Lre") 'hikizan/llm-review-english)
(global-set-key (kbd "C-c e") 'ellama-transient-main-menu)

;; others
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)
(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "M-w") 'kill-ring-save-for-windows)

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
