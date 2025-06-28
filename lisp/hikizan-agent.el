;;; hikizan-agent.el --- agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent functions

;;; Code:

(defun hikizan/run-emacs-agent (workspace)
  "Run the Python Emacs agent in an interactive buffer for a specific WORKSPACE."
  (interactive "DWorkspace: ")
  (let* ((workspace (file-name-as-directory (expand-file-name workspace)))
         (buffer-name (format "*EmacsAgent: %s*"
                              (file-name-nondirectory (directory-file-name workspace))))
         (script-path (expand-file-name "~/.emacs.d/python/emacsagent.py")))
    (let ((default-directory workspace))
      (make-comint-in-buffer "EmacsAgent" buffer-name "python" nil "-u" script-path workspace)
      (pop-to-buffer buffer-name))))

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
        (kill-process (get-buffer-process agent-buffer)))
      (with-current-buffer agent-buffer
        (erase-buffer)
        (hikizan/run-emacs-agent workspace)
        (message "EmacsAgent for %s restarted." workspace)))))

(provide 'hikizan-agent)
