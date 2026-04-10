;;; hikizan-keybinds.el --- Keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for key bindings, evil mode, and which-key

;;; Code:

;;;; Keybindings

;; Toggle windows
(global-set-key (kbd "C-c s") 'scratch-buffer)
(global-set-key (kbd "C-c m") 'hikizan-switch-to-messages-buffer)

;; Scrolling
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-other-window 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-other-window-down 1)))

;; Text scale
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)

;; Navigation
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)

;; LLM tools
(global-set-key (kbd "C-c a w") 'hikizan-run-emacs-agent-web)
(global-set-key (kbd "C-c a r") 'hikizan-emacs-agent-run)
(global-set-key (kbd "C-c a s") 'hikizan-emacs-agent-sessions)

;; Project
(global-set-key (kbd "C-x p g") 'vc-git-grep)

;; Google Translate
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)

;; Hikizan utils
(global-set-key (kbd "C-c c b") 'hikizan-copy-buffer-name)
(global-set-key (kbd "C-c c f") 'hikizan-copy-buffer-file-name)
(global-set-key (kbd "C-c c r") 'hikizan-copy-buffer-file-relative-path)
(global-set-key (kbd "C-c g s") 'hikizan-git-diff-staged)
(global-set-key (kbd "C-c g u") 'hikizan-git-diff)

(provide 'hikizan-keybinds)

;;; hikizan-keybinds.el ends here
