;;; hikizan-project.el --- project  -*- lexical-binding: t; -*-

;; https://github.com/magit/transient
(use-package transient
  :ensure t)

;; https://magit.vc/
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(provide 'hikizan-project)
