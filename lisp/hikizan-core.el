;;; hikizan-util.el --- util -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions

;;; Code:

(require 'project)
(require 'vc-git)
(require 'diff-mode)

(defun hikizan-copy-buffer-name ()
  "Copy buffer name."
  (interactive)
  (kill-new (buffer-name)))

(defun hikizan-copy-buffer-file-name ()
  "Copy buffer file name."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
	(kill-new file-name))))

(defun hikizan-copy-buffer-file-relative-path ()
  "Copy buffer file relative path."
  (interactive)
  (let* ((project (project-current))
	 (project-root (if project (project-root project)))
	 (buffer-file (buffer-file-name))
	 (relative-path (if (and project-root buffer-file)
                            (file-relative-name buffer-file project-root))))
    (when relative-path
      (kill-new relative-path)
      (message "Copied: %s" relative-path))))

(defun hikizan-util--git-diff-buffer (args)
  "Helper to show git diff with ARGS (a list of strings) in a read-only buffer."
  (unless (executable-find "git")
    (error "Git executable not found"))
  
  ;; Prevent generating messy error buffers outside of Git repos
  (unless (vc-git-root default-directory)
    (error "Not inside a Git repository"))

  (let ((cwd default-directory)
        (buffer (get-buffer-create "*hikizan-git-diff*")))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process "git" nil t nil "diff" args)
        (diff-mode)
        (goto-char (point-min))))
        
    ;; Make the buffer read-only and easy to quit with 'q'
    (with-current-buffer buffer
      (setq buffer-read-only t)
      ;; Inherit dynamically instead of deep-copying the keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map diff-mode-map)
        (define-key map (kbd "q") 'quit-window)
        (use-local-map map)))
        
    (pop-to-buffer buffer)))

(defun hikizan-git-diff-staged ()
  "Show git diff --staged in a read-only, dedicated buffer."
  (interactive)
  (hikizan-util--git-diff-buffer '("--staged")))

(defun hikizan-git-diff ()
  "Show git diff in a read-only, dedicated buffer."
  (interactive)
  (hikizan-util--git-diff-buffer nil))

(defun hikizan-switch-to-messages-buffer ()
  "Display the *Messages* buffer in the selected window"
  (interactive)
  (let ((message-buffer (get-buffer "*Messages*")))
    (switch-to-buffer message-buffer)))

;; For EmacsAgent

(defun hikizan-find-string-position-in-buffer (buffer search-string)
  "Find the position of SEARCH-STRING in the specified BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward search-string nil t)
	  (1+ (point))
	nil))))

(defun hikizan-get-string-from-point (buffer point)
  "Get the string from BUFFER starting at POINT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties point (point-max)))))

(defun hikizan-write-string-to-file (filename content)
  "Write CONTENT to FILENAME."
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) filename)))

(defun hikizan-eval-elisp-file (file-path)
  "Evaluate the Elisp code in the specified FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (print file-path)
    (condition-case err
	(eval-buffer)
      (error (message "error: %s" (error-message-string err))))))

;; reading-pacemaker

(defgroup hikizan-reading-pacemaker nil
  "Automatically advance point to pace your reading."
  :group 'convenience)

(defcustom hikizan-reading-pacemaker-interval 0.4
  "Default interval, in seconds, between each word step."
  :type 'number
  :group 'hikizan-reading-pacemaker)

(defvar hikizan-reading-pacemaker-timer nil
  "Timer object for the reading pacemaker.")

(defun hikizan-reading-pacemaker--step ()
  "Move forward one word and recenter the window."
  (forward-word 1)
  (recenter))

(defun hikizan-reading-pacemaker-start (&optional _interval)
  "Start the reading pacemarker.

Every INTERVAL seconds, move forward one word and recenter."
  (interactive
   (list (when current-prefix-arg
	   (read-number
	    (format "Interval between words (seconds) [%s]: " hikizan-reading-pacemaker-interval)
	    hikizan-reading-pacemaker-interval))))
  ;; if already running, cancel first
  (when (timerp hikizan-reading-pacemaker-timer)
    (cancel-timer hikizan-reading-pacemaker-timer))
  (setq hikizan-reading-pacemaker-timer
	(run-with-timer 0 hikizan-reading-pacemaker-interval #'hikizan-reading-pacemaker--step))
  (message "Reading pacemaker started: %s sec/word" hikizan-reading-pacemaker-interval))

(defun hikizan-reading-pacemaker-stop ()
  "Stop the reading pacemaker."
  (interactive)
  (when (timerp hikizan-reading-pacemaker-timer)
    (cancel-timer hikizan-reading-pacemaker-timer)
    (setq hikizan-reading-pacemaker-timer nil)
    (message "Reading pacemaker stopped")))


(defun hikizan-shell-command-to-string-async (command)
  "Execute shell COMMAND asynchronously and return its output as a string.
This avoids blocking the Emacs UI while the command runs."
  (let* ((output "")
         (proc (start-process-shell-command "hikizan-async-shell" nil command)))
    (set-process-filter proc (lambda (_p str) (setq output (concat output str))))
    (while (process-live-p proc)
      (accept-process-output proc 0.1))
    output))

;;; hikizan-editor.el --- editor  -*- lexical-binding: t; -*-

;;; mozc
(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

;;; configurations

;; lockfiles
(setq create-lockfiles nil)

;; character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; backup files
(setq make-backup-files nil)

;; tab / indent
(indent-tabs-mode t)
(setq standard-indent 2)
(setq tab-always-indent 'complete)
(setq tab-width 4)

;; newline / whitespace
(setq require-final-newline t)
(setq show-trailing-whitespace t)
(setq delete-trailing-lines t)

;; line numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; dired
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;; org
(setq org-startup-truncated nil)

;; snippet
(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

;; others
(delete-selection-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(add-to-list 'process-coding-system-alist
             '("grep" . (utf-8-unix . utf-8-unix)))

;;; functions
;;; Code:

(defun hikizan-kill-ring-save-for-windows ()
  "Save the current region to the Windows clipboard via powershell.
This is a workaround for some environments."
  (interactive)
  (if (use-region-p)
      (progn
        (if (executable-find "powershell.exe")
            (let ((text (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
	      (with-temp-buffer
		(insert text)
		(call-process-region (point-min) (point-max) "powershell.exe" nil nil nil
				     "-command"
				     "[Console]::InputEncoding=[Console]::OutputEncoding=[System.Text.Encoding]::UTF8; Add-Type -Assembly PresentationCore; [System.Windows.Clipboard]::SetText([Console]::In.ReadToEnd())")
		(message "Text copied to Windows clipboard")))
          (message "powershell.exe not found"))
        (kill-ring-save (region-beginning) (region-end)))
    (message "No region selected")))

;;; hikizan-ui.el --- ui  -*- lexical-binding: t; -*-

;;; Code:

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package avy
  :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(which-function-mode t)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function #'ignore)


(provide 'hikizan-core)
;;; hikizan-core.el ends here
