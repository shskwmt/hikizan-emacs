;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; treesit
(setq treesit-language-source-alist
   '((cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (element treesit-language-source-alist)
  (let* ((lang (car element)))
    (if (treesit-language-available-p lang)
        (message "treesit: %s is already installed" lang)
      (message "treesit: %s is not installed" lang)
      (treesit-install-language-grammar lang))))

;; ruby
(use-package ruby-ts-mode
  :mode
  (("\\.rb$" . ruby-ts-mode)
   ("\\.rbs$" . ruby-ts-mode)))

;; go
(use-package go-ts-mode
  :mode
  (("\\.go$" . go-ts-mode)
   ("/go\\.mod\\'" . go-mod-ts-mode)))

;; rust
(use-package rust-mode
  :ensure t)

;; yaml
(use-package yaml-ts-mode
  :mode
  (("\\.yaml$" . yaml-ts-mode)
   ("\\.yml$" . yaml-ts-mode)))

;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; terraform
(use-package terraform-mode
  :ensure t)

;; lsp
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (ruby-mode . lsp)
;;   (go-mode . lsp)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   :commands (lsp))

;; (use-package lsp-ui
;;   :ensure t
;;  :commands (lsp-ui-mode))

;;; functions

(defun hikizan/extract-golang-functions ()
  "Extract all Golang function names from the current buffer."
  (interactive)
  (let ((function-names '())
	(regex "^\\(func .*\\)$"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(push (substring-no-properties (match-string 1)) function-names)))
    (let ((new-buffer (generate-new-buffer "*Go Function Names*")))
      (with-current-buffer new-buffer
	(dolist (name (reverse function-names))
	  (insert name "\n"))
	(pop-to-buffer new-buffer)))))

(provide 'hikizan-programming)
