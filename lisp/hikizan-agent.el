;;; hikizan-agent.el --- agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent functions

;;; Code:

(require 'custom)
(require 'comint)
(require 'hikizan-adk-core)

(defgroup hikizan nil
  "Customization group for hikizan."
  :group 'emacs)

(defcustom hikizan-agent-web-port "3333"
  "Port for the Emacs Agent web interface."
  :type 'string
  :group 'hikizan)

(defcustom hikizan-agent-python-dir (expand-file-name "~/.emacs.d/python/")
  "Directory containing agent scripts."
  :type 'directory
  :group 'hikizan)

(defcustom hikizan-agent-db-name "emacs_agent_sessions.db"
  "Filename for the Emacs Agent session database."
  :type 'string
  :group 'hikizan)

(setq message-log-max 1000000)

(defvar hikizan-agent-mode-map
  (let ((map (make-keymap)))
    
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

(defun hikizan--wait-for-web-server (url port attempts &optional skip-browser)
  "Wait for the web server at PORT to be ready, then open URL.
ATTEMPTS is the maximum number of seconds to wait."
  (let ((port-num (if (stringp port) (string-to-number port) port)))
    (if (condition-case nil
            (let ((proc (make-network-process :name "hikizan-port-check"
                                              :host "localhost"
                                              :service port-num
                                              :noquery t)))
              (when proc
                (delete-process proc)
                t))
          (error nil))
        (progn
          (unless skip-browser (browse-url url))
          (message "Emacs Agent Web is ready."))
      (if (> attempts 0)
          (run-with-timer 1 nil #'hikizan--wait-for-web-server url port (1- attempts) skip-browser)
        (message "Emacs Agent Web server didn't respond at port %s. Check the log buffer." port)))))

(defun hikizan/run-emacs-agent-web (&optional skip-browser action)
  "Run the Emacs agent with a web interface using AGENTS_DIR and session persistence.
Dynamically waits for the server to be ready before opening the browser."
  (interactive)
  (unless (executable-find "adk")
    (error "Command 'adk' not found in PATH"))
  (let* ((workspace (expand-file-name "~"))
         (agents-dir hikizan-agent-python-dir)
         (port hikizan-agent-web-port)
         (db-path (expand-file-name hikizan-agent-db-name workspace))
         (uri (format "sqlite:///%s" db-path))
         (url (format "http://localhost:%s" port))
         (buffer-name "*EmacsAgent-Web: Home*")
         (buf (get-buffer-create buffer-name))
         (proc (get-buffer-process buf)))

    (when (and (process-live-p proc)
               (called-interactively-p 'interactive)
               (not action))
      (setq action (pcase (car (read-multiple-choice
                                 "Emacs Agent Web is already running"
                                 '((?o "open")
                                   (?r "restart")
                                   (?q "quit"))))
                     (?o 'open)
                     (?r 'restart)
                     (?q 'quit))))

    (pcase action
      ('quit (message "Aborted."))
      ('restart
       (when (process-live-p proc)
         (kill-process proc)
         (while (process-live-p proc)
           (accept-process-output proc 0.1)))
       (hikizan/run-emacs-agent-web skip-browser 'start))
      ('open
       (unless skip-browser (browse-url url))
       (message "Opened Emacs Agent Web in browser."))
      (_
       (if (not (process-live-p proc))
           (progn
             (pop-to-buffer buf)
             (with-current-buffer buf
               (let ((default-directory workspace))
                 (when proc (delete-process proc)) ;; Clean up dead process
                 (erase-buffer)
                 (let ((new-proc (start-process "EmacsAgent-Web" (current-buffer)
                                                "adk" "web" agents-dir "--port" port "--session_service_uri" uri)))
                   (set-process-sentinel new-proc
                                         (lambda (p event)
                                           (message "EmacsAgent-Web process %s: %s" p (string-trim event))))
                   (message "Starting Emacs Agent Web... Waiting for initialization.")
                   (hikizan--wait-for-web-server url port 20 skip-browser)))))
         (unless skip-browser (browse-url url))
         (message "Emacs Agent Web is already running."))))))


(defvar hikizan-emacs-agent-dir (expand-file-name "emacs_agent" hikizan-agent-python-dir)
  "Path to the Emacs agent.")

(defun hikizan/emacs-agent-run ()
  "Run the Emacs agent."
  (interactive)
  (hikizan/adk-run hikizan-emacs-agent-dir))

(defun hikizan/emacs-agent-resume (session-file)
  "Resume an Emacs agent session from SESSION-FILE."
  (interactive
   (list (read-file-name "Session file: " hikizan-emacs-agent-dir nil t nil
                        (lambda (f) (or (file-directory-p f) (string-match-p "\\.session\\.json$" f))))))
  (hikizan/adk-resume hikizan-emacs-agent-dir session-file))

(defun hikizan/emacs-agent-replay (session-file)
  "Replay an Emacs agent session from SESSION-FILE."
  (interactive
   (list (read-file-name "Session file: " hikizan-emacs-agent-dir nil t nil
                        (lambda (f) (or (file-directory-p f) (string-match-p "\\.session\\.json$" f))))))
  (hikizan/adk-replay hikizan-emacs-agent-dir session-file))

(defun hikizan/emacs-agent-sessions ()
  "Show the dashboard for the Emacs agent."
  (interactive)
  (hikizan/adk-sessions hikizan-emacs-agent-dir))
(provide 'hikizan-agent)

;;; hikizan-agent.el ends here
