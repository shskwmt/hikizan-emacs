;;; hikizan-adk-ui.el --- Dashboard UI for ADK -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard UI for ADK

;;; Code:

(require 'tabulated-list)
(require 'hikizan-adk-process)
(require 'seq)
(require 'subr-x)
(require 'json)

(defvar hikizan-adk--agent-path)
(defvar hikizan-adk--session-id)

(defvar-local hikizan-adk--dashboard-agent-path nil
  "Path to the agent directory for the current dashboard.")
(defvar-local hikizan-adk--dashboard-sessions-path nil
  "Path where the dashboard looks for sessions.")

(defcustom hikizan-adk-ui-refresh-interval 30
  "Interval in seconds to refresh the ADK sessions dashboard."
  :type 'integer
  :group 'hikizan-adk)

(defvar-local hikizan-adk-ui--refresh-timer nil
  "Timer for automatic refresh of the ADK sessions dashboard.")

(define-derived-mode hikizan-adk-ui-mode tabulated-list-mode "Hikizan-ADK-Dashboard"
  "Major mode for ADK sessions dashboard."
  (setq tabulated-list-format [("State" 10 t)
                               ("Plan" 6 t)
                               ("Plan Title" 40 t)
                               ("PID" 10 t)
                               ("Updated" 20 t)
                               ("Summary" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'hikizan-adk-ui--refresh-entries nil t)
  (add-hook 'kill-buffer-hook #'hikizan-adk-ui--stop-refresh-timer nil t)
  (tabulated-list-init-header)
  (hikizan-adk-ui--start-refresh-timer))

(define-key hikizan-adk-ui-mode-map (kbd "RET") #'hikizan-adk-ui-resume)
(define-key hikizan-adk-ui-mode-map (kbd "g") #'hikizan-adk-ui-refresh)
(define-key hikizan-adk-ui-mode-map (kbd "k") #'hikizan-adk-ui-kill-session)
(define-key hikizan-adk-ui-mode-map (kbd "D") #'hikizan-adk-ui-delete-session)
(define-key hikizan-adk-ui-mode-map (kbd "o") #'hikizan-adk-ui-open-session-file)
(define-key hikizan-adk-ui-mode-map (kbd "P") #'hikizan-adk-ui-open-plan)

(defun hikizan-adk-ui--plan-exists-p (session-id &optional sessions-path)
  "Return a list of plan files for SESSION-ID, sorted by name.
Use SESSIONS-PATH if provided, otherwise use the dashboard default."
  (let* ((clean-id (if (and session-id
                            (string-match "\\.session$" session-id))
                       (replace-match "" t t session-id)
                     session-id))
         (session-dir (expand-file-name
                       (concat (or clean-id "") "/")
                       (or sessions-path hikizan-adk--dashboard-sessions-path)))
         (pattern "^plan.*\\.org$"))
    (if (and clean-id (file-directory-p session-dir))
        (sort (directory-files session-dir t pattern) #'string-lessp)
      nil)))

(defun hikizan-adk-ui--get-session-id (id)
  "Extract the session ID from the dashboard row ID.
ID can be a buffer (running session) or a string path (saved session)."
  (cond
   ((bufferp id)
    (with-current-buffer id hikizan-adk--session-id))
   ((stringp id)
    (if (string= (file-name-nondirectory id) "session.json")
        (file-name-nondirectory (directory-file-name (file-name-directory id)))
      (file-name-base id)))
   (t nil)))

(defun hikizan-adk-ui--get-buffer-summary (buffer)
  "Extract the second user message from the ADK comint BUFFER.
If there is only one user message, use that."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((user-messages '())
            (prompt-regexp "^\\[user\\]: "))
        (while (re-search-forward prompt-regexp nil t)
          (let ((msg (buffer-substring-no-properties (point) (line-end-position))))
            (setq msg (string-trim msg))
            (when (> (length msg) 0)
              (push msg user-messages))))
        (setq user-messages (nreverse user-messages))
        (let ((text (or (nth 1 user-messages) (nth 0 user-messages))))
          (if (and text (> (length text) 0))
              (let ((first-line (car (split-string text "\n"))))
                (if (> (length first-line) 60)
                    (concat (substring (string-trim first-line) 0 57) "...")
                  (string-trim first-line)))
            "No summary"))))))

(defun hikizan-adk-ui--get-session-summary (file-path)
  "Extract the second user message from session JSON at FILE-PATH.
If there is only one user message, use that."
  (condition-case err
      (let* ((json-object (with-temp-buffer
                            (insert-file-contents file-path)
                            (json-parse-string (buffer-string) :object-type 'alist)))
             (events (cdr (assoc 'events json-object)))
             (user-messages '())
             (text nil))
        (when (vectorp events)
          (seq-doseq (event events)
            (let* ((author (cdr (assoc 'author event)))
                   (content (cdr (assoc 'content event)))
                   (parts (cdr (assoc 'parts content)))
                   (first-part (and (vectorp parts) (> (length parts) 0) (aref parts 0)))
                   (msg-text (cdr (assoc 'text first-part))))
              (when (and (string= author "user") msg-text)
                (push msg-text user-messages)))))
        (setq user-messages (nreverse user-messages))
        (setq text (or (nth 1 user-messages) (nth 0 user-messages)))
        (if (and text (> (length text) 0))
            (let ((first-line (car (split-string text "\n"))))
              (if (> (length first-line) 60)
                  (concat (substring (string-trim first-line) 0 57) "...")
                (string-trim first-line)))
          "No summary"))
    (error (format "Error: %s" (error-message-string err)))))

(defun hikizan-adk-ui--get-plan-title (file-path)
  "Extract the title from the Org plan file at FILE-PATH.
It looks for #+TITLE: or the first first-level heading."
  (when (and file-path (file-exists-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (cond
       ((re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
        (match-string 1))
       ((re-search-forward "^\\* \\(.*\\)$" nil t)
        (match-string 1))
       (t "No title")))))

(defun hikizan-adk-ui--refresh-entries ()
  "Refresh the list of sessions for the current dashboard agent."
  (let* ((agent-path hikizan-adk--dashboard-agent-path)
         (sessions-base hikizan-adk--dashboard-sessions-path)
         (entries nil))
    (when agent-path
      ;; 1. Running/Stopped buffer sessions
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (derived-mode-p 'hikizan-adk-run-mode)
                     hikizan-adk--agent-path
                     (file-equal-p hikizan-adk--agent-path agent-path))
            (let* ((proc (get-buffer-process buf))
                   (live (and proc (process-live-p proc)))
                   (state (if live "running" "stopped" ))
                   (session-id hikizan-adk--session-id)
                   (plans (hikizan-adk-ui--plan-exists-p session-id sessions-base))
                   (plan-count (length plans))
                   (plan-str (cond ((= plan-count 0) "   ")
                                   ((= plan-count 1) "[Y]")
                                   (t (format "[%d]" plan-count))))
                   (plan-title (if plans (hikizan-adk-ui--get-plan-title (car plans)) ""))
                   (pid (if live (format "%d" (process-id proc)) "-" ))
                   (session-file (expand-file-name (concat session-id "/session.json") sessions-base))
                   (exists (file-exists-p session-file))
                   (updated (cond
                             (exists (format-time-string "%Y-%m-%d %H:%M" (nth 5 (file-attributes session-file))))
                             (live (format-time-string "%Y-%m-%d %H:%M" (current-time)))
                             (t "")))
                   (summary (cond
                             (exists (hikizan-adk-ui--get-session-summary session-file))
                             (t (hikizan-adk-ui--get-buffer-summary buf)))))
              (push (list buf (vector state plan-str plan-title pid updated summary)) entries)))))

      ;; 2. Saved session files
      (when (and sessions-base (file-directory-p sessions-base))
        (dolist (session-dir (directory-files sessions-base t "^[^.]"))
          (when (file-directory-p session-dir)
            (let* ((session-id (file-name-nondirectory session-dir))
                   (session-file (expand-file-name "session.json" session-dir)))
              (when (file-exists-p session-file)
                (let* ((attrs (file-attributes session-file))
                       (mtime (nth 5 attrs))
                       (updated (format-time-string "%Y-%m-%d %H:%M" mtime))
                       (plans (hikizan-adk-ui--plan-exists-p session-id sessions-base))
                       (plan-count (length plans))
                       (plan-str (cond ((= plan-count 0) "   ")
                                       ((= plan-count 1) "[Y]")
                                       (t (format "[%d]" plan-count))))
                       (plan-title (if plans (hikizan-adk-ui--get-plan-title (car plans)) ""))
                       (summary (hikizan-adk-ui--get-session-summary session-file)))
                  (push (list session-file (vector "saved" plan-str plan-title "-" updated summary)) entries))))))))
    (setq tabulated-list-entries entries)))

(defun hikizan-adk-ui--start-refresh-timer ()
  "Start the automatic refresh timer for the current buffer."
  (hikizan-adk-ui--stop-refresh-timer)
  (setq hikizan-adk-ui--refresh-timer
        (run-at-time hikizan-adk-ui-refresh-interval
                     hikizan-adk-ui-refresh-interval
                     (lambda (buf)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (hikizan-adk-ui-refresh))))
                     (current-buffer))))

(defun hikizan-adk-ui--stop-refresh-timer ()
  "Stop the automatic refresh timer for the current buffer."
  (when hikizan-adk-ui--refresh-timer
    (cancel-timer hikizan-adk-ui--refresh-timer)
    (setq hikizan-adk-ui--refresh-timer nil)))

(defun hikizan-adk-ui-refresh ()
  "Refresh the dashboard."
  (interactive)
  (revert-buffer))

(defun hikizan-adk-ui-resume ()
  "Perform action on current session."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((bufferp id)
      (pop-to-buffer id))
     ((stringp id)
      (hikizan-adk-ui--resume-from-file id))
     (t (message "No action for this row.")))))

(defun hikizan-adk-ui-kill-session ()
  "Kill the process of the selected running session."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (bufferp id)
        (with-current-buffer id
          (hikizan-adk-kill)
          (hikizan-adk-ui-refresh))
      (message "Not a running session buffer."))))

(defun hikizan-adk-ui-delete-session ()
  "Delete the selected saved session file and its directory."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((stringp id)
      (let* ((session-id (hikizan-adk-ui--get-session-id id))
             (session-dir (file-name-directory id)))
        (when (yes-or-no-p (format "Delete session %s and all its files? " session-id))
          (if (and (file-directory-p session-dir)
                   (not (file-equal-p session-dir hikizan-adk--dashboard-sessions-path)))
              (progn
                (delete-directory session-dir t)
                (message "Deleted session directory %s" session-dir))
            (delete-file id)
            (message "Deleted session file %s" id))
          (hikizan-adk-ui-refresh))))
     ((bufferp id)
      (message "Selected item is a buffer. Use 'k' to kill its process."))
     (t
      (message "No session selected.")))))

(defun hikizan-adk-ui-open-dashboard (agent-path &optional sessions-path)
  "Open ADK sessions dashboard for AGENT-PATH.
If SESSIONS-PATH is provided, use it as the base directory for sessions."
  (let ((buffer (get-buffer-create "*hikizan-adk-sessions*")))
    (with-current-buffer buffer
      (hikizan-adk-ui-mode)
      (setq hikizan-adk--dashboard-agent-path (expand-file-name agent-path))
      (setq hikizan-adk--dashboard-sessions-path
            (or (and sessions-path (expand-file-name sessions-path))
                (let ((env-path (getenv "EMACS_AGENT_SESSIONS_BASE_DIR")))
                  (when env-path (expand-file-name env-path)))
                (expand-file-name "sessions/" agent-path)))
      (hikizan-adk-ui-refresh)
      (pop-to-buffer buffer))))

(defun hikizan-adk-ui--resume-from-file (file-path)
  "Resume an ADK session from FILE-PATH."
  (let ((agent-path hikizan-adk--dashboard-agent-path))
    (hikizan-adk--run-process agent-path (list "--resume" file-path))))

(defun hikizan-adk-ui-open-session-file ()
  "Open the session file at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (stringp id)
        (find-file id)
      (message "Not a saved session file."))))

(defun hikizan-adk-ui-open-plan ()
  "Open the planned task Org-Mode file for the current session.
If multiple plans exist, prompt for selection.
With a prefix argument, open all plans."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (sessions-path hikizan-adk--dashboard-sessions-path)
         (session-id (hikizan-adk-ui--get-session-id id))
         (plans (hikizan-adk-ui--plan-exists-p session-id sessions-path)))
    (if plans
        (cond
         (current-prefix-arg
          (dolist (file plans)
            (pop-to-buffer (find-file-noselect file))))
         ((= (length plans) 1)
          (pop-to-buffer (find-file-noselect (car plans))))
         (t
          (let* ((choices (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) plans))
                 (selected (completing-read "Select plan: " choices nil t)))
            (when (and selected (not (string-empty-p selected)))
              (pop-to-buffer (find-file-noselect (cdr (assoc selected choices))))))))
      (message "No task plans found for session: %s" session-id))))

(provide 'hikizan-adk-ui)
;;; hikizan-adk-ui.el ends here
