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

(defvar hikizan-agent-font-lock-keywords
  '(;; Markdown-like highlighting
    ("^#+ .*" . font-lock-type-face)                     ; Headers
    ("\\*\\*\\(.*?\\)\\*\\*" 1 'bold)                    ; Bold
    ("`\\(.*?\\)`" 1 font-lock-string-face)              ; Inline code
    ("^\\s-*[-+*]\\s-+" . font-lock-variable-name-face)) ; List bullets
  "Font lock keywords for hikizan-agent-mode.")

(define-minor-mode hikizan-agent-mode
  "A minor mode for Emacs Agent buffers."
  :init-value nil
  :lighter " Agent"
  :keymap hikizan-agent-mode-map
  (if hikizan-agent-mode
      (font-lock-add-keywords nil hikizan-agent-font-lock-keywords)
    (font-lock-remove-keywords nil hikizan-agent-font-lock-keywords))
  (font-lock-flush))

(defun hikizan/start-emacs-agent-process (buffer-name workspace)
  "Start the comint process for the Emacs agent.
Use BUFFER-NAME and WORKSPACE."
  (let ((script-path (expand-file-name "~/.emacs.d/python/emacsagent"))
        ;; Force Python to use UTF-8 for stdout/stderr
        (process-environment (cons "PYTHONIOENCODING=utf-8" process-environment)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((default-directory workspace))
        ;; Use 'adk run' as the entry point for the agent package
        (make-comint-in-buffer "EmacsAgent" buffer-name "adk" nil "run" script-path)
        ;; Set the coding system for the process in this buffer
        (let ((proc (get-buffer-process (current-buffer))))
          (when proc
            (set-process-coding-system proc 'utf-8 'utf-8)))
        (hikizan-agent-mode 1)))))

(defun hikizan/run-emacs-agent (workspace)
  "Run the Python Emacs agent in an interactive buffer for a specific WORKSPACE."
  (interactive "DWorkspace: ")
  (let* ((workspace (file-name-as-directory (expand-file-name workspace)))
         (default-directory workspace)
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
        (sleep-for 1))
      (with-current-buffer agent-buffer
        (erase-buffer))
      (hikizan/start-emacs-agent-process buffer-name workspace)
      (message "EmacsAgent for %s restarted." workspace))))

(provide 'hikizan-agent)

;;; hikizan-agent.el ends here
