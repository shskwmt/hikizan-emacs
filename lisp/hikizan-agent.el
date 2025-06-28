;;; hikizan-agent.el --- agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent functions

;;; Code:

(setq message-log-max 1000000)

(defvar hikizan-agent-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c r") #'hikizan/restart-emacs-agent)
    map)
  "Keymap for hikizan-agent-mode.")

(define-minor-mode hikizan-agent-mode
  "A minor mode for Emacs Agent buffers."
  :init-value nil
  :lighter " Agent"
  :keymap hikizan-agent-mode-map)

(defun hikizan/start-emacs-agent-process (buffer-name workspace)
  "Starts the comint process for the Emacs agent."
  (let ((script-path (expand-file-name "~/.emacs.d/python/emacsagent.py")))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((default-directory workspace))
        (make-comint-in-buffer "EmacsAgent" buffer-name "python" nil "-u" script-path workspace)
        (hikizan-agent-mode 1)))))

(defun hikizan/run-emacs-agent (workspace)
  "Run the Python Emacs agent in an interactive buffer for a specific WORKSPACE."
  (interactive "DWorkspace: ")
  (let* ((workspace (file-name-as-directory (expand-file-name workspace)))
         (buffer-name (format "*EmacsAgent: %s*"
                              (file-name-nondirectory (directory-file-name workspace)))))
    (pop-to-buffer buffer-name)
    (hikizan/start-emacs-agent-process buffer-name workspace)))

(defun hikizan/restart-emacs-agent ()
  "Restart the Emacs agent in the buffer corresponding to the current workspace."
  (interactive)
  (let* ((workspace (file-name-as-directory (expand-file-name default-directory)))
         (buffer-name (format "*EmacsAgent: %s*"
                              (file-name-nondirectory (directory-file-name workspace))))
         (agent-buffer (get-buffer buffer-name)))
    (if (not agent-buffer)
        (message "No EmacsAgent buffer found for workspace: %s" workspace)
      (when (get-buffer-process agent-buffer)
        (kill-process (get-buffer-process agent-buffer))
        (sleep-for 3))
      (with-current-buffer agent-buffer
        (erase-buffer))
      (hikizan/start-emacs-agent-process buffer-name workspace)
      (message "EmacsAgent for %s restarted." workspace))))

(provide 'hikizan-agent)
