;;; hikizan-adk-process.el --- Process runner for ADK -*- lexical-binding: t; -*-

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'server)

(defgroup hikizan-adk nil
  "ADK integration for Hikizan Emacs."
  :group 'external)

(defcustom hikizan-adk-command "adk"
  "The adk command to run."
  :type 'string
  :group 'hikizan-adk)

(defvar-local hikizan-adk--agent-path nil)
(defvar-local hikizan-adk--session-id nil)
(defvar-local hikizan-adk--client-buffer nil)

(defun hikizan-adk--generate-session-id ()
  "Generate a session ID in the format hikizan-YYYYMMDD-HHMMSS."
  (format-time-string "hikizan-%Y%m%d-%H%M%S"))

(defun hikizan-adk--start-daemon (session-id)
  "Start a new Emacs daemon with SESSION-ID as the server name."
  (message "Starting dedicated Emacs daemon: %s" session-id)
  (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
         (daemon-name session-id)
         (daemon-buf (format " *emacs-daemon-%s*" daemon-name))
         (proc (start-process (format "emacs-daemon-%s" daemon-name)
                              daemon-buf emacs-bin
                              (format "--fg-daemon=%s" daemon-name))))
    ;; 1. ensure daemon process won't block Emacs exit
    (set-process-query-on-exit-flag proc nil)

    ;; 2. wait until daemon is ready
    (let ((timeout 120))
      (while (and (> timeout 0)
                  (not (server-running-p daemon-name))
                  (process-live-p proc))
        (sleep-for 0.5)
        (setq timeout (1- timeout))))

    (unless (server-running-p daemon-name)
      (let ((output (if (get-buffer daemon-buf)
                        (with-current-buffer daemon-buf (buffer-string))
                      "No output buffer found.")))
        (error "Failed to start Emacs daemon: %s. Output:\n%s" daemon-name output)))

    ;; 3. start emacsclient
    (let* ((command
            (format "%s %s %s -t"
                    (if (eq system-type 'windows-nt)
			"emacsclientw"
                      "emacsclient")
                    (if (eq system-type 'windows-nt) "-f" "-s")
                    session-id))
	   (shell-command-buffer-name-async
            (format "*hikizan-emacsclient:%s*" session-id)))
      (message "Launching emacsclient via async-shell-command: %s" command)
      (async-shell-command command)
      (setq hikizan-adk--client-buffer shell-command-buffer-name-async))))

(defun hikizan-adk--kill-daemon (session-id)
  "Kill the Emacs daemon with SESSION-ID."
  (when (and session-id (server-running-p session-id))
    (call-process "emacsclient" nil nil nil
                  (if (eq system-type 'windows-nt) "-f" "-s")
                  session-id "-e" "(kill-emacs)")))

(defun hikizan-adk--process-sentinel (proc _event)
  "Sentinel for ADK process to cleanup the daemon and client."
  (let ((buf (process-buffer proc)))
    (when (and (buffer-live-p buf)
               (memq (process-status proc) '(exit signal)))
      (with-current-buffer buf
        (when hikizan-adk--session-id
          (message "Cleaning up Emacs daemon for session: %s"
                   hikizan-adk--session-id)
          (hikizan-adk--kill-daemon hikizan-adk--session-id))
	(hikizan-adk--kill-client-if-needed)))))

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
	(progn
          (when hikizan-adk--session-id
            (hikizan-adk--kill-daemon hikizan-adk--session-id))
          (delete-process proc))
      (message "No active process to kill."))))

(defun hikizan-adk--kill-daemon-if-needed ()
  "Kill Emacs daemon associated with current ADK session."
  (when hikizan-adk--session-id
    (message "Cleaning up Emacs daemon (buffer kill): %s"
             hikizan-adk--session-id)
    (hikizan-adk--kill-daemon hikizan-adk--session-id)))

(defun hikizan-adk--kill-client-if-needed ()
  "Kill emacsclient process and its buffer if present."
  (sleep-for 0.5)
  (when hikizan-adk--client-buffer
    (when (buffer-live-p hikizan-adk--client-buffer)
      (kill-buffer hikizan-adk--client-buffer))
    (setq hikizan-adk--client-buffer nil)))

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
  (setq-local font-lock-defaults '(hikizan-adk-font-lock-keywords))
  (add-hook 'kill-buffer-hook
          (lambda ()
            (hikizan-adk--kill-daemon-if-needed)
            (hikizan-adk--kill-client-if-needed))
          nil t))

(defun hikizan-adk--run-process (agent-path &optional extra-args new-session)
  "Run ADK in AGENT-PATH with EXTRA-ARGS.
If NEW-SESSION is non-nil, force starting a new session even if an
active one exists."
  (let* ((agent-path (expand-file-name agent-path))
         (agent-name (file-name-nondirectory (directory-file-name agent-path)))
         (active-buf (unless new-session
                       (cl-find-if (lambda (buf)
                                     (with-current-buffer buf
                                       (and (derived-mode-p 'hikizan-adk-run-mode)
                                            hikizan-adk--agent-path
                                            (file-equal-p hikizan-adk--agent-path agent-path)
                                            (let ((proc (get-buffer-process buf)))
                                              (and proc (process-live-p proc))))))
                                   (buffer-list)))))
    (if active-buf
        (pop-to-buffer active-buf)
      (let* ((session-file (cadr (member "--replay" extra-args)))
             (session-id (if session-file
                             (file-name-base (file-name-sans-extension session-file))
                           (hikizan-adk--generate-session-id)))
             (buffer-name (format "*hikizan-adk:%s:%s*" agent-name session-id))
             (session-uri "memory://")
             (args (append (list "run"
                                 "--save_session"
                                 "--session_id" session-id
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
              
              ;; 1. Start the dedicated daemon
              (hikizan-adk--start-daemon session-id)
              
              ;; 2. Set the environment variable for the child process
              (let ((process-environment (cons (format "EMACS_SERVER_FILE=%s" session-id)
                                               process-environment)))
                (apply #'make-comint-in-buffer "adk" (current-buffer) hikizan-adk-command nil args))
              
              ;; 3. Set the sentinel for cleanup
              (let ((new-proc (get-buffer-process (current-buffer))))
                (when new-proc
                  (set-process-sentinel new-proc #'hikizan-adk--process-sentinel)))
              
              (pop-to-buffer (current-buffer)))))))))

(provide 'hikizan-adk-process)
;;; hikizan-adk-process.el ends here
