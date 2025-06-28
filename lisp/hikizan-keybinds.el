;;; hikizan-keybinds.el --- Keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for key bindings, evil mode, and which-key

;;; Code:

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
(global-set-key (kbd "C-c a") 'hikizan/run-emacs-agent)

;; Consult
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-s d") 'consult-fd)
(global-set-key (kbd "M-s g") 'consult-git-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)

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

;;; evil Configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; change mode-line color by evil state
  (let ((default-color (cons (face-background 'mode-line)
                             (face-foreground 'mode-line))))
    (add-hook 'post-command-hook
	      (lambda ()
		(let ((color (cond ((minibufferp) default-color)
				   ((evil-insert-state-p) '("#e80000" . "#ffffff"))
				   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
				   ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
				   (t default-color))))
		  (set-face-background 'mode-line (car color))
		  (set-face-foreground 'mode-line (cdr color))))))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(provide 'hikizan-keybinds)

;;; hikizan-keybinds.el ends here
