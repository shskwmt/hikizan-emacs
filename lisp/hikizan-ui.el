;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

;;; Code:

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package avy
  :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

(provide 'hikizan-ui)
;;; hikizan-ui.el ends here
