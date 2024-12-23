;;; hikizan-editor.el --- editor  -*- lexical-binding: t; -*-

;; lockfiles
(setq create-lockfiles nil)

;; character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; backup files
(setq make-backup-files nil)

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
(setq clm/command-log-buffer (get-buffer-create " *command-log*"))
(defun hikizan/save-command-log ()
  (if (bufferp clm/command-log-buffer)
      (clm/save-command-log)))

(use-package command-log-mode
  :ensure t
  :config
  (setopt clm/log-command-exceptions* '(nil self-insert-command newline))
  (global-command-log-mode t)
  (add-hook 'kill-emacs-hook 'clm/save-command-log)
  (add-function :after after-focus-change-function (lambda () (clm/save-command-log))))

;; editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; dired
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;; org
(setq org-startup-truncated nil)

;; others
(delete-selection-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)

(provide 'hikizan-editor)
