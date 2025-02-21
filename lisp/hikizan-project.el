;;; hikizan-project.el --- project  -*- lexical-binding: t; -*-

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(recentf-mode +1)

(provide 'hikizan-project)
