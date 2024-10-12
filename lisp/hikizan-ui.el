;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-light t))

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
