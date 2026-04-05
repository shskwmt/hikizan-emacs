;;; hikizan-adk-process.el --- Process runner for ADK -*- lexical-binding: t; -*-

(require 'comint)
(require 'cl-lib)

(defgroup hikizan-adk nil
  "ADK integration for Hikizan Emacs."
  :group 'external)

(defcustom hikizan-adk-command "adk"
  "The adk command to run."
  :type 'string
  :group 'hikizan-adk)

(defvar-local hikizan-adk--agent-path nil)
(defvar-local hikizan-adk--backend-profile 'sqlite)
(defvar-local hikizan-adk--session-service-uri nil)
(defvar-local hikizan-adk--session-id nil)

(defun hikizan/adk--generate-session-id ()
  "Generate a session ID in the format hikizan-YYYYMMDD-HHMMSS."
  (format-time-string "hikizan-%Y%m%d-%H%M%S"))

(defun hikizan/adk--ensure-adk-dir (agent-path)
  "Ensure the sessions directory exists in AGENT-PATH."
  (let ((adk-dir (expand-file-name "sessions" agent-path)))
    (unless (file-directory-p adk-dir)
      (make-directory adk-dir t))
    adk-dir))

(defun hikizan/adk-exit ()
  "Send exit command to the ADK process."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (comint-send-string proc "exit\n")
      (message "No active process to exit."))))

(defun hikizan/adk-kill ()
  "Kill the ADK process."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (delete-process proc)
      (message "No active process to kill."))))

(defvar hikizan-adk-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'hikizan/adk-exit)
    (define-key map (kbd "C-c C-k") #'hikizan/adk-kill)
    map)
  "Keymap for `hikizan-adk-run-mode'.")

(define-derived-mode hikizan-adk-run-mode comint-mode "Hikizan-ADK"
  "Major mode for running ADK sessions."
  (setq comint-prompt-regexp "^> ")
  (setq-local comint-use-prompt-regexp t))

(defun hikizan/adk--run-process (agent-path &optional extra-args new-session)
  "Run ADK in AGENT-PATH with EXTRA-ARGS.
If NEW-SESSION is non-nil, rename the existing buffer if it has a live process."
  (let* ((agent-path (expand-file-name agent-path))
         (agent-name (file-name-nondirectory (directory-file-name agent-path)))
         (buffer-name (format "*hikizan-adk:%s*" agent-name))
         (existing-buf (get-buffer buffer-name)))
    (when (and new-session existing-buf (process-live-p (get-buffer-process existing-buf)))
      (with-current-buffer existing-buf
        (rename-buffer (format "*hikizan-adk:%s:%s*" agent-name (or hikizan-adk--session-id "old")) t)))
    (let* ((adk-dir (hikizan/adk--ensure-adk-dir agent-path))
           (db-path (expand-file-name "session.db" adk-dir))
           (session-id (hikizan/adk--generate-session-id))
           (sqlite-uri (format "sqlite:///%s" db-path))
           (args (append (list "run"
                               "--save_session"
                               "--session_id" session-id
                               "--session_service_uri" sqlite-uri)
                         extra-args
                         (list agent-path))))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq default-directory adk-dir)
        (unless (derived-mode-p 'hikizan-adk-run-mode)
          (hikizan-adk-run-mode))
        (let ((proc (get-buffer-process (current-buffer))))
          (if (and proc (process-live-p proc))
              (pop-to-buffer (current-buffer))
            (setq hikizan-adk--agent-path agent-path)
            (setq hikizan-adk--session-id session-id)
            (setq hikizan-adk--session-service-uri sqlite-uri)
            (apply #'make-comint-in-buffer "adk" (current-buffer) hikizan-adk-command nil args)
            (pop-to-buffer (current-buffer))))))))

(provide 'hikizan-adk-process)
