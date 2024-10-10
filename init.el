;;; init.el --- -*- lexical-binding: t; -*-

(defvar hikizan-dir (file-name-directory load-file-name))
(defvar hikizan-lisp-dir (expand-file-name "lisp" hikizan-dir))

(add-to-list 'load-path hikizan-lisp-dir)

(require 'hikizan-package-manager)
(require 'hikizan-project)
(require 'hikizan-editor)
(require 'hikizan-completion)
(require 'hikizan-org)
(require 'hikizan-snippet)
(require 'hikizan-programming)
(require 'hikizan-ui)
(require 'hikizan-keybinds)

(setq system-time-local "C") ;; to avoid Japanese in the time stamp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window ellama command-log-mode terraform-mode go-mode consult zenburn-theme bind-map corfu orderless vertico yasnippet-snippets yasnippet git-gutter magit which-key evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
