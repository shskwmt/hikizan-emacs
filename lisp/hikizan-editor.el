;;; hikizan-editor.el --- editor  -*- lexical-binding: t; -*-

;; lockfiles
(setq create-lockfiles nil)

;; character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; backup files
(setq make-backup-files t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; tab / indent
(indent-tabs-mode t)
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

;; command logs
(use-package command-log-mode
  :ensure t
  :config
  (setopt clm/log-command-exceptions* '(nil self-insert-command newline))
  (add-hook 'kill-emacs-hook 'clm/save-command-log)
  (add-function :after after-focus-change-function (lambda () (clm/save-command-log)))
  (global-command-log-mode t))

;; dired
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;; others
(delete-selection-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)

(provide 'hikizan-editor)
