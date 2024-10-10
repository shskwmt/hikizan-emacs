;;; hikizan-keybinds.el --- keybinds -*- lexical-binding: t; -*-

;;; unset keys

;; function
(global-unset-key (kbd "C-<f10>")) ;; buffer-menu-open
(global-unset-key (kbd "S-<f10>")) ;; context-menu-open
(global-unset-key (kbd "<f10>")) ;; menu-bar-open

;; C-
(global-unset-key (kbd "C-z")) ;; suspend-frame
(global-unset-key (kbd "C-<down>")) ;; forward-paragraph
(global-unset-key (kbd "C-<insert>")) ;; kill-ring-save
(global-unset-key (kbd "C-<insertchar>")) ;; kill-ring-save
(global-unset-key (kbd "C-<up>")) ;; backward-paragraph

;; C-M-
(global-unset-key (kbd "C-M-<backspace>")) ;; backward-kill-sexp
(global-unset-key (kbd "C-M-<delete>")) ;; backward-kill-sexp
(global-unset-key (kbd "C-M-<down>")) ;; down-list
(global-unset-key (kbd "C-M-<end>")) ;; end-of-defun
(global-unset-key (kbd "C-M-<home>")) ;; beginning-of-defun
(global-unset-key (kbd "C-M-<up>")) ;; backward-up-list

;; C-x
(global-unset-key (kbd "C-x C-d")) ;; list-directory
(global-unset-key (kbd "C-x C-z")) ;; suspend-frame

;; M-
(global-unset-key (kbd "M-`")) ;; tmm-menubar

;;; packages

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

;; https://github.com/justbur/emacs-bind-map
(use-package bind-map
  :ensure t)

;;; hikezan keybinds

(defconst hikizan-leader-key "M-x")

(defvar hikizan-leader-map (make-sparse-keymap)
  "Base keymap for hikizan-emacs leader key commands.")

(defun hikizan/declare-prefix (prefix name &rest more)
  "Declare a prefix PREFIX. PREFIX is a string describing a key sequence."
  (declare (indent defun))
  (apply #'which-key-add-keymap-based-replacements hikizan-leader-map
	 prefix name more))

(defun hikizan/bind-map-set-key (key map label)
  "Bind a given MAP to the KEY."
  (bind-map-set-keys hikizan-leader-map key map)
  (hikizan/declare-prefix key label))

;;; leader

(bind-map hikizan-leader-map
  :keys (hikizan-leader-key))

(bind-map-set-keys hikizan-leader-map
  "SPC" 'execute-extended-command
  "TAB" 'consult-buffer
  "c" 'clm/toggle-command-log-buffer
  "g" 'consult-goto-line
  "l" 'consult-line
  "y" 'consult-yank-from-kill-ring)

;;; ai
(defvar hikizan-ai-map (make-sparse-keymap)
  "AI keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-ai-map
  "a" 'ellama-ask-about
  "c" 'ellama-chat
  "r" 'ellama-code-review
  "s" 'ellama-summarize)

(hikizan/bind-map-set-key "a" hikizan-ai-map "ai")

;;; buffer
(defvar hikizan-buffer-map (make-sparse-keymap)
  "Buffer keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-buffer-map
  "l" 'list-buffers
  "d" 'kill-buffer
  "r" 'rename-buffer
  "R" 'revert-buffer
  "s" 'consult-buffer)

(hikizan/bind-map-set-key "b" hikizan-buffer-map "buffer")

;;; bookmark
(defvar hikizan-bookmark-map (make-sparse-keymap)
  "Bookmark keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-bookmark-map
  "j" 'consult-bookmark
  "l" 'bookmark-bmenu-list
  "s" 'bookmark-set)

(hikizan/bind-map-set-key "B" hikizan-bookmark-map "bookmark")

;;; eval
(defvar hikizan-eval-map (make-sparse-keymap)
  "Eval keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-eval-map
  "b" 'eval-buffer
  "l" 'eval-last-sexp
  "r" 'eval-region)

(hikizan/bind-map-set-key "e" hikizan-eval-map "eval")

;;; file
(defvar hikizan-file-map (make-sparse-keymap)
  "File keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-file-map
  "f" 'find-file
  "r" 'rgrep)

(hikizan/bind-map-set-key "f" hikizan-file-map "file")

;;; org
(defvar hikizan-org-map (make-sparse-keymap)
  "Org keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-org-map
  "a" 'org-agenda
  "c" 'org-capture
  "o" 'consult-org-agenda)

(hikizan/bind-map-set-key "o" hikizan-org-map "org")

;;; project
(defvar hikizan-project-map (make-sparse-keymap)
  "Project keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-project-map
  "b" 'consult-project-buffer
  "c" 'project-compile
  "d" 'project-dired
  "e" 'project-eshell
  "f" 'project-find-file
  "p" 'project-switch-project
  "r" 'consult-ripgrep)

(hikizan/bind-map-set-key "p" hikizan-project-map "project")

;;; register
(defvar hikizan-register-map (make-sparse-keymap)
  "Register keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-register-map
  "p" 'point-to-register
  "j" 'consult-register
  "c" 'copy-to-register
  "i" 'insert-register)

(hikizan/bind-map-set-key "r" hikizan-register-map "register")

;;; snippet
(defvar hikizan-snippet-map (make-sparse-keymap)
  "Snippet keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-snippet-map
  "n" 'yas-new-snippet
  "o" 'yas-visit-snippet-file
  "i" 'yas-insert-snippet)

(hikizan/bind-map-set-key "s" hikizan-snippet-map "snippet")

;;; window
(defvar hikizan-window-map (make-sparse-keymap)
  "Window keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-window-map
  "d" 'delete-window
  "m" 'delete-other-windows
  "h" 'windmove-left
  "l" 'windmove-right
  "k" 'windmove-up
  "j" 'windmove-down
  "/" 'split-window-right
  "-" 'split-window-below)

(hikizan/bind-map-set-key "w" hikizan-window-map "window")

;;; global key bindings
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-h C-i") 'consult-info)

;;; mode key bindings

;;; eshell-mode

(defun eshell-mode-keybinds-hook ()
  (local-set-key (kbd "C-c h") 'consult-history))
(add-hook 'eshell-mode-hook 'eshell-mode-keybinds-hook)

(provide 'hikizan-keybinds)
