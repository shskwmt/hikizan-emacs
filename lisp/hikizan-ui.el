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

(menu-bar-mode -1)
(tool-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)

;; prevent to create a new window
(setq display-buffer-base-action '((display-buffer-same-window)))

;;; popup windows

(use-package popwin
  :ensure t)

;; util functions

(defun hikizan/get-or-create-buffer-from-file-name (file-name)
  "Return the buffer associated with the given FILE-NAME.
If no such buffer exists, open the file and create a new buffer."
  (let ((buffer (get-file-buffer file-name)))
    (if buffer
        buffer
      (find-file file-name)
      (get-file-buffer file-name))))

(defun hikizan/get-window-from-file-name (file-name)
  "Return the window associated with the given FILE-NAME, or nil if no such window exists."
  (get-buffer-window (hikizan/get-or-create-buffer-from-file-name file-name)))

(defun hikizan/open-dedicated-window-bottom (buffer height &optional noselect tail)
  "Open BUFFER with dedicated popup window bottom. HEIGHT is the height of window."
  (popwin:popup-buffer buffer
		       :noselect noselect
		       :tail tail
		       :dedicated t
		       :stick t
		       :height height))

;; buffer-list window

(defun hikizan/get-buffer-list-window ()
  "Get the *buffer-list* window."
  (get-buffer-window (list-buffers-noselect)))

(defun hikizan/open-buffer-list-window ()
  "Open the *buffer-list* window."
  (interactive)
  (let ((window (hikizan/get-buffer-list-window)))
    (unless (windowp window)
      (hikizan/open-dedicated-window-bottom (list-buffers-noselect) 0.4))))

(defun hikizan/close-buffer-list-window ()
  "Close the *buffer-list* window."
  (interactive)
  (let ((window (hikizan/get-buffer-list-window)))
    (if (windowp window)
	(delete-window window))))

(defun hikizan/toggle-buffer-list-window ()
  "Toggle the *buffer-list* window."
  (interactive)
  (let ((window (hikizan/get-buffer-list-window)))
    (if (windowp window)
	(hikizan/close-buffer-list-window)
      (hikizan/open-buffer-list-window))))

;; scratch window

(defun hikizan/get-scratch-buffer ()
  "Get the *scratch* buffer."
  (get-buffer "*scratch*"))

(defun hikizan/get-scratch-window ()
  "Get the *scratch* window."
  (get-buffer-window (hikizan/get-scratch-buffer)))

(defun hikizan/open-scratch-window ()
  "Open the *scratch* window."
  (interactive)
  (let ((window (hikizan/get-scratch-window)))
    (unless (windowp window)
      (hikizan/open-dedicated-window-bottom (hikizan/get-scratch-buffer) 0.4))))

(defun hikizan/close-scratch-window ()
  "Close the *scratch* window."
  (interactive)
  (let ((window (hikizan/get-scratch-window)))
    (if (windowp window)
	(delete-window window))))

(defun hikizan/toggle-scratch-window ()
  "Toggle the *scratch* window."
  (interactive)
  (let ((window (hikizan/get-scratch-window)))
    (if (windowp window)
	(hikizan/close-scratch-window)
      (hikizan/open-scratch-window))))

;; org note window

(defun hikizan/open-org-note-window ()
  "Open the org note window."
  (interactive)
  (let ((window (hikizan/get-window-from-file-name org-default-notes-file)))
    (unless (windowp window)
      (hikizan/open-dedicated-window-bottom (hikizan/get-or-create-buffer-from-file-name org-default-notes-file) 0.4))))

(defun hikizan/close-org-note-window ()
  "Close the org note window."
  (interactive)
  (let ((window (hikizan/get-window-from-file-name org-default-notes-file)))
    (if (windowp window)
	(delete-window window))))

(defun hikizan/toggle-org-note-window ()
  "Toggle the org note window."
  (interactive)
  (let ((window (hikizan/get-window-from-file-name org-default-notes-file)))
    (if (windowp window)
	(hikizan/close-org-note-window)
      (hikizan/open-org-note-window))))

;; message window
(defun hikizan/get-messages-buffer ()
  "Get the *Message* buffer."
  (get-buffer "*Messages*"))

(defun hikizan/get-messages-window ()
  "Get the *Messages* window."
  (get-buffer-window (hikizan/get-messages-buffer)))

(defun hikizan/open-messages-window ()
  "Open the *Messages* window."
  (interactive)
  (let ((window (hikizan/get-messages-window)))
    (unless (windowp window)
      (hikizan/open-dedicated-window-bottom (hikizan/get-messages-buffer) 0.4 t t))))

(defun hikizan/close-messages-window ()
  "Close the *Messages* window."
  (interactive)
  (let ((window (hikizan/get-messages-window)))
    (if (windowp window)
	(delete-window window))))

(defun hikizan/toggle-messages-window ()
  "Toggle the *Messages* window."
  (interactive)
  (let ((window (hikizan/get-messages-window)))
    (if (windowp window)
	(hikizan/close-messages-window)
      (hikizan/open-messages-window))))

(provide 'hikizan-ui)
;;; hikizan-ui.el ends here
