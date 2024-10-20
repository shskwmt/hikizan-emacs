;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(font . "NotoMono NF-12.0"))
(setq use-default-font-for-symbols nil)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icon-corfu
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-black t))

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
