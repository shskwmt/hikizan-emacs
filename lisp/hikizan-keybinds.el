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
(global-set-key (kbd "C-c j") 'avy-goto-char)
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

;; Reading pacemaker
(global-set-key (kbd "C-c r s") 'hikizan-reading-pacemaker-start)
(global-set-key (kbd "C-c r t") 'hikizan-reading-pacemaker-stop)

;; Google Translate
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)

;; Hikizan utils
(global-set-key (kbd "C-c c b") 'hikizan-copy-buffer-name)
(global-set-key (kbd "C-c c f") 'hikizan-copy-buffer-file-name)
(global-set-key (kbd "C-c c r") 'hikizan-copy-buffer-file-relative-path)
(global-set-key (kbd "C-c g s") 'hikizan-git-diff-staged)
(global-set-key (kbd "C-c g u") 'hikizan-git-diff)

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
