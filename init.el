;;; init.el --- -*- lexical-binding: t; -*-
;;(setq debug-on-error t)

;;; Code:

(defvar hikizan-dir (file-name-directory load-file-name))
(defvar hikizan-lisp-dir (expand-file-name "lisp" hikizan-dir))

(add-to-list 'load-path hikizan-lisp-dir)
(add-to-list 'load-path (expand-file-name "hikizan-adk" hikizan-lisp-dir))

(require 'hikizan-package-manager)
(require 'hikizan-core)
(require 'hikizan-adk-core)
(require 'hikizan-programming)
(require 'hikizan-agent)
(require 'hikizan-keybinds)

(setq system-time-local "C") ;; to avoid Japanese in the time stamp

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
