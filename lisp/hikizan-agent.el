;;; hikizan-agent.el --- agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent functions

;;; Code:

(require 'custom)
(require 'comint)
(require 'hikizan-adk-core)
(require 'subr-x)
(require 'pcase)

(defgroup hikizan-agent nil
  "Customization group for hikizan agent."
  :group 'emacs)

(defcustom hikizan-agent-python-dir (expand-file-name "~/.emacs.d/python/")
  "Directory containing agent scripts."
  :type 'directory
  :group 'hikizan-agent)

(setq message-log-max 1000000)

(defvar hikizan-emacs-agent-dir (expand-file-name "emacs_agent" hikizan-agent-python-dir)
  "Path to the Emacs agent.")

(defun hikizan-emacs-agent-run ()
  "Run the Emacs agent.  Always start a new session."
  (interactive)
  (hikizan-adk-run hikizan-emacs-agent-dir))

(defun hikizan-emacs-agent-sessions ()
  "Show the dashboard for the Emacs agent."
  (interactive)
  (hikizan-adk-sessions hikizan-emacs-agent-dir))

;; Functions for Agent

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

(defun hikizan-shell-command-to-string-async (command)
  "Execute shell COMMAND asynchronously and return its output as a string.
This avoids blocking the Emacs UI while the command runs."
  (let* ((output "")
         (proc (start-process-shell-command "hikizan-async-shell" nil command)))
    (set-process-filter proc (lambda (_p str) (setq output (concat output str))))
    (while (process-live-p proc)
      (accept-process-output proc 0.1))
    output))

(provide 'hikizan-agent)

;;; hikizan-agent.el ends here
