;;; hikizan-snippet.el --- snippet  -*- lexical-binding: t; -*-

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

(provide 'hikizan-snippet)
