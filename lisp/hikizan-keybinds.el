;;; hikizan-keybinds.el --- keybinds -*- lexical-binding: t; -*-

;;; unset keys

;; function
(global-unset-key (kbd "C-<f10>")) ;; buffer-menu-open
(global-unset-key (kbd "S-<f10>")) ;; context-menu-open
(global-unset-key (kbd "<f10>")) ;; menu-bar-open

;; C-
(global-unset-key (kbd "C-z")) ;; suspend-frame
(global-unset-key (kbd "C-<down>")) ;; forward-paragraph
(global-unset-key (kbd "C-<insert>")) ;; kill-ring-save
(global-unset-key (kbd "C-<insertchar>")) ;; kill-ring-save
(global-unset-key (kbd "C-<up>")) ;; backward-paragraph

;; C-M-
(global-unset-key (kbd "C-M-<backspace>")) ;; backward-kill-sexp
(global-unset-key (kbd "C-M-<delete>")) ;; backward-kill-sexp
(global-unset-key (kbd "C-M-<down>")) ;; down-list
(global-unset-key (kbd "C-M-<end>")) ;; end-of-defun
(global-unset-key (kbd "C-M-<home>")) ;; beginning-of-defun
(global-unset-key (kbd "C-M-<up>")) ;; backward-up-list

;; C-x
(global-unset-key (kbd "C-x C-d")) ;; list-directory
(global-unset-key (kbd "C-x C-z")) ;; suspend-frame

;; M-
(global-unset-key (kbd "M-`")) ;; tmm-menubar

;;; packages

;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot 10)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(provide 'hikizan-keybinds)
