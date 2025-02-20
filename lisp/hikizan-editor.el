;;; hikizan-editor.el --- editor  -*- lexical-binding: t; -*-

;;; configurations

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

;;; command logs
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

;;; functions
(defun kill-ring-save-for-windows ()
  "Copy region to the Windows clipboard using clip.exe"
  (interactive)
  (if (use-region-p)
      (progn
	(if (executable-find "clip.exe")
	    (let ((text (buffer-substring-no-properties
			 (region-beginning)
			 (region-end))))
	      (with-temp-buffer
		(insert text)
		(call-process-region (point-min) (point-max) "clip.exe")))
	  (message "clip.exe not found"))
	(kill-ring-save (region-beginning) (region-end)))
    (message "No region selected")))

(provide 'hikizan-editor)
