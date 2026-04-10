;;; init.el --- Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration entry point.

;;; Code:

;; Add lisp directories to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/hikizan-adk" user-emacs-directory))

;; Initialize packages
(require 'hikizan-package-manager)

;; Load core
(require 'hikizan-core)
(require 'hikizan-agent)
(require 'hikizan-programming)
(require 'hikizan-keybinds)

(provide 'init)
;;; init.el ends here
