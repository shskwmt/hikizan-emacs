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
(defvar-local hikizan-adk--backend-profile 'json)
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

(defun hikizan/adk--start-daemon (session-id)
  "Start a new Emacs daemon with SESSION-ID as the server name."
  (message "Starting dedicated Emacs daemon: %s" session-id)
  (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
         (daemon-name session-id)
         (proc (start-process (format "emacs-daemon-%s" daemon-name)
                              nil emacs-bin
                              (format "--daemon=%s" daemon-name)))
         (timeout 40))
    (while (and (> timeout 0) (not (server-running-p daemon-name)))
      (sleep-for 0.5)
      (setq timeout (1- timeout)))
    (unless (server-running-p daemon-name)
      (error "Failed to start Emacs daemon: %s" daemon-name))))

(defun hikizan/adk--kill-daemon (session-id)
  "Kill the Emacs daemon with SESSION-ID."
  (when (and session-id (server-running-p session-id))
    (call-process "emacsclient" nil nil nil
                  (if (eq system-type 'windows-nt) "-f" "-s")
                  session-id "-e" "(kill-emacs)")))

(defun hikizan/adk--process-sentinel (proc event)
  "Sentinel for ADK process to cleanup the daemon and manage session files."
  (let ((buf (process-buffer proc)))
    (when (and (buffer-live-p buf)
               (memq (process-status proc) '(exit signal)))
      (let ((session-id (with-current-buffer buf hikizan-adk--session-id))
            (agent-path (with-current-buffer buf hikizan-adk--agent-path)))
        (when session-id
          (message "Cleaning up Emacs daemon for session: %s" session-id)
          (hikizan/adk--kill-daemon session-id)
          ;; Move session file from AGENT/ to AGENT/sessions/ if it was saved
          (when agent-path
            (let ((old-path (expand-file-name (format "%s.session.json" session-id) agent-path))
                  (new-path (expand-file-name (format "sessions/%s.session.json" session-id) agent-path)))
              (when (and (not (file-equal-p old-path new-path))
                         (file-exists-p old-path))
                (rename-file old-path new-path t)
                (message "Moved session file to %s" new-path))))))))
  )

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
	(progn
          (when hikizan-adk--session-id
            (hikizan/adk--kill-daemon hikizan-adk--session-id))
          (delete-process proc))
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
    (define-key map (kbd "C-c C-q") #'hikizan/adk-exit)
    (define-key map (kbd "C-c C-k") #'hikizan/adk-kill)
    map)
  "Keymap for `hikizan-adk-run-mode'.")

(define-derived-mode hikizan-adk-run-mode comint-mode "Hikizan-ADK"
  "Major mode for running ADK sessions."
  (setq comint-prompt-regexp "^> ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local font-lock-defaults '(hikizan-adk-font-lock-keywords)))

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
           (session-file (or (cadr (member "--resume" extra-args))
                             (cadr (member "--replay" extra-args))))
           (session-id (if session-file
                           (file-name-base (file-name-sans-extension session-file))
                         (hikizan/adk--generate-session-id)))
           (session-uri "memory://")
           (args (append (list "run"
                               "--save_session"
                               "--session_id" session-id
                               "--session_service_uri" session-uri)
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
            (setq hikizan-adk--session-service-uri session-uri)
            
            ;; 1. Start the dedicated daemon
            (hikizan/adk--start-daemon session-id)
            
            ;; 2. Set the environment variable for the child process
            (let ((process-environment (cons (format "EMACS_SERVER_FILE=%s" session-id)
                                             process-environment)))
              (apply #'make-comint-in-buffer "adk" (current-buffer) hikizan-adk-command nil args))
            
            ;; 3. Set the sentinel for cleanup
            (let ((new-proc (get-buffer-process (current-buffer))))
              (when new-proc
                (set-process-sentinel new-proc #'hikizan/adk--process-sentinel)))
            
            (pop-to-buffer (current-buffer))))))))

(provide 'hikizan-adk-process)
