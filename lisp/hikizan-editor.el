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
  (global-command-log-mode t)
  (run-with-timer 60 60 'clm/save-command-log))

;; others
(delete-selection-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)

(provide 'hikizan-editor)
