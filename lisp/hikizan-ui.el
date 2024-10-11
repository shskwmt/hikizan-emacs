;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

(use-package zenburn-theme
  :ensure t
  :init
  (setq zenburn-use-variable-pitch t
	zenburn-scale-org-headlines t
	zenburn-scale-outline-headlines t)
  :config
  (load-theme 'zenburn t))

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

(provide 'hikizan-ui)
