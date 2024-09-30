;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

(load-theme 'modus-vivendi)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

(provide 'hikizan-ui)
