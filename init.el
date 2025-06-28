;;; init.el --- -*- lexical-binding: t; -*-
;;(setq debug-on-error t)

(defvar hikizan-dir (file-name-directory load-file-name))
(defvar hikizan-lisp-dir (expand-file-name "lisp" hikizan-dir))

(add-to-list 'load-path hikizan-lisp-dir)

(require 'hikizan-package-manager)
(require 'hikizan-util)
(require 'hikizan-completion)
(require 'hikizan-project)
(require 'hikizan-editor)
(require 'hikizan-org)
(require 'hikizan-snippet)
(require 'hikizan-programming)
(require 'hikizan-agent)
(require 'hikizan-ui)
(require 'hikizan-keybinds)

(setq system-time-local "C") ;; to avoid Japanese in the time stamp

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(server-start)
