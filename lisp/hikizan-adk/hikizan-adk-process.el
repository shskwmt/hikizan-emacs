;;; hikizan-adk-process.el --- Process runner for ADK -*- lexical-binding: t; -*-

;;; Commentary:
;; Process runner for ADK

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'server)

(defgroup hikizan-adk nil
  "ADK integration for Hikizan Emacs."
  :group 'external)

(defcustom hikizan-adk-command "python"
  "The adk command to run."
  :type 'string
  :group 'hikizan-adk)

(defvar-local hikizan-adk--agent-path nil
  "Path to the agent directory for the current session.")
(defvar-local hikizan-adk--session-id nil
  "The session identifier for the current session.")

(defun hikizan-adk--generate-session-id ()
  "Generate a session ID in the format hikizan-YYYYMMDD-HHMMSS."
  (format-time-string "hikizan-%Y%m%d-%H%M%S"))

(defun hikizan-adk--process-sentinel (proc _event)
  "Sentinel for ADK process PROC to cleanup."
  (let ((buf (process-buffer proc)))
    (when (and (buffer-live-p buf)
               (memq (process-status proc) '(exit signal)))
      (message "ADK process finished: %s" (process-name proc)))))

(defun hikizan-adk-exit ()
  "Send exit command to the ADK process."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (comint-send-string proc "exit\n")
      (message "No active process to exit."))))

(defun hikizan-adk-kill ()
  "Kill the ADK process."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (delete-process proc)
      (message "No active process to kill."))))

(defvar hikizan-adk-font-lock-keywords
  '(;; Markdown-like highlighting
    ("^#+ .*" . font-lock-type-face)                     ; Headers
    ("\\*\\*\\(.*?\\)\\*\\*" 1 'bold)                    ; Bold
    ("`\\(.*?\\)`" 1 font-lock-string-face)              ; Inline code
    ("^\\s-*[-+*]\\s-+" . font-lock-variable-name-face)) ; List bullets
  "Font lock keywords for `hikizan-adk-run-mode'.")

(defvar hikizan-adk-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'hikizan-adk-exit)
    (define-key map (kbd "C-c C-k") #'hikizan-adk-kill)
    map)
  "Keymap for `hikizan-adk-run-mode'.")

(define-derived-mode hikizan-adk-run-mode comint-mode "Hikizan-ADK"
  "Major mode for running ADK sessions."
  (setq comint-prompt-regexp "^> ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local font-lock-defaults '(hikizan-adk-font-lock-keywords)))

(defun hikizan-adk--run-process (agent-path &optional extra-args new-session)
  "Run ADK in AGENT-PATH with EXTRA-ARGS.
If NEW-SESSION is non-nil, force starting a new session even if an
active one exists."
  (let* ((agent-path (expand-file-name agent-path))
         (agent-name (file-name-nondirectory (directory-file-name agent-path)))
         (session-file (or (cl-second (member "--replay" extra-args))
                           (cl-second (member "--resume" extra-args))))
         (target-session-id (when session-file
                              (if (string= (file-name-nondirectory session-file) "session.json")
                                  (file-name-nondirectory (directory-file-name (file-name-directory session-file)))
                                (file-name-base (file-name-sans-extension session-file)))))
         (active-buf (unless new-session
                       (cl-find-if (lambda (buf)
                                     (with-current-buffer buf
                                       (and (derived-mode-p 'hikizan-adk-run-mode)
                                            hikizan-adk--agent-path
                                            (file-equal-p hikizan-adk--agent-path agent-path)
                                            (or (not target-session-id)
                                                (string= hikizan-adk--session-id target-session-id))
                                            (let ((proc (get-buffer-process buf)))
                                              (and proc (process-live-p proc))))))
                                   (buffer-list)))))
    (if active-buf
        (pop-to-buffer active-buf)
      (let* ((session-id (or target-session-id
                             (hikizan-adk--generate-session-id)))
             (buffer-name (format "*hikizan-adk:%s:%s*" agent-name session-id))
             (session-uri "memory://")
             (use-python-m (string-match-p "python" hikizan-adk-command))
             (args (append (if use-python-m
                               (list "-m" "emacs_agent.main")
                             (list "run"))
                           (list "--save_session"
                                 "--session_id" session-id
                                 "--emacs_server_file_dir" (expand-file-name server-auth-dir)
                                 "--session_service_uri" session-uri)
                           extra-args
                           (list agent-path))))
        (with-current-buffer (get-buffer-create buffer-name)
          (unless (derived-mode-p 'hikizan-adk-run-mode)
            (hikizan-adk-run-mode))
          (let ((proc (get-buffer-process (current-buffer))))
            (if (and proc (process-live-p proc))
                (pop-to-buffer (current-buffer))
              (setq hikizan-adk--agent-path agent-path)
              (setq hikizan-adk--session-id session-id)
              
              (let ((coding-system-for-read 'utf-8)
                    (coding-system-for-write 'utf-8)
                    (default-directory (if use-python-m
                                           (file-name-directory (directory-file-name agent-path))
                                         default-directory)))
                (apply #'make-comint-in-buffer "adk" (current-buffer) hikizan-adk-command nil args))
              
              (let ((new-proc (get-buffer-process (current-buffer))))
                (when new-proc
                  (set-process-sentinel new-proc #'hikizan-adk--process-sentinel)))
              
              (pop-to-buffer (current-buffer)))))))))

(provide 'hikizan-adk-process)
;;; hikizan-adk-process.el ends here
