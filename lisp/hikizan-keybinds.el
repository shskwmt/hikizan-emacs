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
	which-key-idle-delay 0.05)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; https://github.com/justbur/emacs-bind-map
(use-package bind-map
  :ensure t)

;;; hikezan keybinds

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
  :keys ("M-SPC")
  :evil-keys ("SPC")
  :evil-states (normal motion visual))

(bind-map-set-keys hikizan-leader-map
  "SPC" 'execute-extended-command
  "TAB" 'switch-to-buffer)

;; override leader key
(add-hook 'dired-mode-hook
	  (lambda ()
	    (bind-map-set-keys dired-mode-map "SPC" hikizan-leader-map)))
(add-hook 'magit-status-mode-hook
	  (lambda ()
	    (bind-map-set-keys magit-status-mode-map "SPC" hikizan-leader-map)))
(add-hook 'Info-mode-hook
	  (lambda ()
	    (bind-map-set-keys Info-mode-map "SPC" hikizan-leader-map)))

;;; buffer
(defvar hikizan-buffer-map (make-sparse-keymap)
  "Buffer keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-buffer-map
  "l" 'list-buffers
  "d" 'kill-buffer
  "s" 'switch-to-buffer)

(hikizan/bind-map-set-key "b" hikizan-buffer-map "buffer")

;;; file
(defvar hikizan-file-map (make-sparse-keymap)
  "File keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-file-map
  "f" 'find-file)

(hikizan/bind-map-set-key "f" hikizan-file-map "file")

;;; project
(defvar hikizan-project-map (make-sparse-keymap)
  "Project keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-project-map
  "b" 'project-switch-to-buffer
  "c" 'project-compile
  "d" 'project-dired
  "e" 'project-eshell
  "f" 'project-find-file
  "p" 'project-switch-project)

(hikizan/bind-map-set-key "p" hikizan-project-map "project")

;;; git

(defvar hikizan-git-map (make-sparse-keymap)
  "Git keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-git-map
  "m" 'magit-status
  "g" 'vc-git-grep)

(hikizan/bind-map-set-key "g" hikizan-git-map "git")

;;; window

(defvar hikizan-window-map (make-sparse-keymap)
  "Window keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-window-map
  "d" 'delete-window
  "h" 'windmove-left
  "l" 'windmove-right
  "k" 'windmove-up
  "j" 'windmove-down
  "/" 'split-window-right
  "-" 'split-window-below)

(hikizan/bind-map-set-key "w" hikizan-window-map "window")

;;; bookmark

(defvar hikizan-bookmark-map (make-sparse-keymap)
  "Bookmark keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-bookmark-map
  "j" 'bookmark-jump
  "l" 'bookmark-bmenu-list
  "s" 'bookmark-set)

(hikizan/bind-map-set-key "B" hikizan-bookmark-map "bookmark")

;;; register

(defvar hikizan-register-map (make-sparse-keymap)
  "Register keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-register-map
  "p" 'point-to-register
  "j" 'jump-to-register
  "c" 'copy-to-register
  "i" 'insert-register)

(hikizan/bind-map-set-key "r" hikizan-register-map "register")

;;; org

(defvar hikizan-org-map (make-sparse-keymap)
  "Org keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-org-map
  "a" 'org-agenda
  "c" 'org-capture)

(hikizan/bind-map-set-key "o" hikizan-org-map "org")

;;; snippet

(defvar hikizan-snippet-map (make-sparse-keymap)
  "Snippet keymap for hikizan-emacs.")

(bind-map-set-keys hikizan-snippet-map
  "n" 'yas-new-snippet
  "o" 'yas-visit-snippet-file
  "i" 'yas-insert-snippet)

(hikizan/bind-map-set-key "s" hikizan-snippet-map "snippet")

(provide 'hikizan-keybinds)
