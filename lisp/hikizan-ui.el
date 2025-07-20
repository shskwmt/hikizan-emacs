;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

;;; Code:

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(add-to-list 'default-frame-alist '(font . "NotoMono NF-12.0"))
(setq use-default-font-for-symbols nil)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package avy
  :ensure t)

(use-package ace-window
  :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

;;; util functions

(defun hikizan/switch-to-messages-buffer ()
  "Display the *Messages* buffer in the selected window"
  (interactive)
  (let ((message-buffer (get-buffer "*Messages*")))
    (switch-to-buffer message-buffer)))

(provide 'hikizan-ui)
;;; hikizan-ui.el ends here
