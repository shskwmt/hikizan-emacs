;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

;;; Code:

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

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
