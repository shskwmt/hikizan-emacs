;;; early-init.el --- -*- lexical-binding: t; -*-

;; set environment variables
(defvar env-file-path "~/.emacsenv")
(if (file-readable-p env-file-path)
  (load-file env-file-path))

;; disable package.el
(setq package-enable-at-startup nil)
