;;; hikizan-programming.el --- programming  -*- lexical-binding: t; -*-

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ruby
(use-package ruby-mode
  :ensure t)

;; go
(use-package go-mode
  :ensure t)

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
