;;; hikizan-core.el --- util -*- lexical-binding: t; -*-

;;; Commentary:
;; Core features

;;; Code:

;;; Require
(require 'project)
(require 'vc-git)
(require 'diff-mode)
(require 'dired)
(require 'which-key)
(require 'display-line-numbers)
(require 'em-hist)

;;; Packages
(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

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

;;; Utility Functions

(defun hikizan-copy-buffer-name ()
  "Copy buffer name."
  (interactive)
  (kill-new (buffer-name)))

(defun hikizan-copy-buffer-file-name ()
  "Copy buffer file name."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
	(kill-new file-name))))

(defun hikizan-copy-buffer-file-relative-path ()
  "Copy buffer file relative path."
  (interactive)
  (let* ((project (project-current))
	 (project-root (if project (project-root project)))
	 (buffer-file (buffer-file-name))
	 (relative-path (if (and project-root buffer-file)
                            (file-relative-name buffer-file project-root))))
    (when relative-path
      (kill-new relative-path)
      (message "Copied: %s" relative-path))))

(defun hikizan-git-diff-buffer (args)
  "Helper to show git diff with ARGS (a list of strings) in a read-only buffer."
  (unless (executable-find "git")
    (error "Git executable not found"))
  
  ;; Prevent generating messy error buffers outside of Git repos
  (unless (vc-git-root default-directory)
    (error "Not inside a Git repository"))

  (let ((cwd default-directory)
        (buffer (get-buffer-create "*hikizan-git-diff*")))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process "git" nil t nil "diff" args)
        (diff-mode)
        (goto-char (point-min))))
        
    ;; Make the buffer read-only and easy to quit with 'q'
    (with-current-buffer buffer
      (setq buffer-read-only t)
      ;; Inherit dynamically instead of deep-copying the keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map diff-mode-map)
        (define-key map (kbd "q") 'quit-window)
        (use-local-map map)))
        
    (pop-to-buffer buffer)))

(defun hikizan-git-diff-staged ()
  "Show git diff --staged in a read-only, dedicated buffer."
  (interactive)
  (hikizan-git-diff-buffer '("--staged")))

(defun hikizan-git-diff ()
  "Show git diff in a read-only, dedicated buffer."
  (interactive)
  (hikizan-git-diff-buffer nil))

(defun hikizan-switch-to-messages-buffer ()
  "Display the *Messages* buffer in the selected window."
  (interactive)
  (let ((message-buffer (get-buffer "*Messages*")))
    (switch-to-buffer message-buffer)))

;;; configurations

;; lockfiles
(setq create-lockfiles nil)

;; character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; backup files
(setq make-backup-files nil)

;; tab / indent
(setq-default indent-tabs-mode t)
(setq standard-indent 2)
(setq tab-always-indent 'complete)
(setq tab-width 4)

;; newline / whitespace
(setq require-final-newline t)
(setq show-trailing-whitespace t)
(setq delete-trailing-lines t)

;; line numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; dired
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)

;; eshell
(setq eshell-history-size 10000
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-history-append t)

;; org
(setq org-startup-folded nil)
(with-eval-after-load 'org
  (require 'ox-md))

;; others
(delete-selection-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(add-to-list 'process-coding-system-alist
             '("grep" . (utf-8-unix . utf-8-unix)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(which-function-mode t)

(recentf-mode +1)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

;; Window Management
(setq split-height-threshold nil
      split-width-threshold nil
      switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist
      '((".*" (display-buffer-same-window))))

(provide 'hikizan-core)
;;; hikizan-core.el ends here
