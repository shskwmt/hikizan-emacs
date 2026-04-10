;;; hikizan-adk-ui.el --- Dashboard UI for ADK -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard UI for ADK

;;; Code:

(require 'tabulated-list)
(require 'hikizan-adk-process)

(defvar-local hikizan-adk--dashboard-agent-path nil)

(defcustom hikizan-adk-ui-refresh-interval 5
  "Interval in seconds to refresh the ADK sessions dashboard."
  :type 'integer
  :group 'hikizan-adk)

(defvar-local hikizan-adk-ui--refresh-timer nil
  "Timer for automatic refresh of the ADK sessions dashboard.")

(define-derived-mode hikizan-adk-ui-mode tabulated-list-mode "Hikizan-ADK-Dashboard"
  "Major mode for ADK sessions dashboard."
  (setq tabulated-list-format [("State" 10 t)
                               ("Plan" 6 t)
                               ("Name" 30 t)
                               ("PID" 10 t)
                               ("Updated" 20 t)
                               ("Summary" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'hikizan-adk-ui--refresh-entries nil t)
  (add-hook 'kill-buffer-hook #'hikizan-adk-ui--stop-refresh-timer nil t)
  (tabulated-list-init-header)
  (hikizan-adk-ui--start-refresh-timer))

(define-key hikizan-adk-ui-mode-map (kbd "RET") #'hikizan-adk-ui-action)
(define-key hikizan-adk-ui-mode-map (kbd "g") #'hikizan-adk-ui-refresh)
(define-key hikizan-adk-ui-mode-map (kbd "k") #'hikizan-adk-ui-kill-session)
(define-key hikizan-adk-ui-mode-map (kbd "D") #'hikizan-adk-ui-delete-session)
(define-key hikizan-adk-ui-mode-map (kbd "o") #'hikizan-adk-ui-open-file)
(define-key hikizan-adk-ui-mode-map (kbd "P") #'hikizan-adk-ui-open-planned-task)

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

(defun hikizan-adk-ui--refresh-entries ()
  "Refresh the list of sessions for the current dashboard agent."
  (let* ((agent-path hikizan-adk--dashboard-agent-path)
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
                   (has-plan (hikizan-adk-ui--plan-exists-p session-id))
                   (pid (if live (format "%d" (process-id proc)) "-" )))
              (push (list buf (vector state (if has-plan "[Y]" "   " ) (buffer-name buf) pid "" "-" )) entries)))))

      ;; 2. Saved session files
      (when (file-directory-p agent-path)
        (let* ((files (directory-files agent-path t "\\.session\\.json$" )))
          (dolist (file files)
            (let* ((name (file-name-nondirectory file))
                   (attrs (file-attributes file))
                   (mtime (nth 5 attrs))
                   (updated (format-time-string "%Y-%m-%d %H:%M" mtime))
                   (session-id (file-name-base file))
                   (has-plan (hikizan-adk-ui--plan-exists-p session-id))
                   (summary (hikizan-adk-ui--get-session-summary file)))
              (push (list file (vector "saved" (if has-plan "[Y]" "   " ) name "-" updated summary)) entries))))))
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

(defun hikizan-adk-ui-action ()
  "Perform action on current session."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((bufferp id)
      (pop-to-buffer id))
     ((stringp id)
      (hikizan-adk--resume-from-file id))
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
  "Delete the selected saved session file."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((stringp id)
      (when (yes-or-no-p (format "Delete session file %s? " (file-name-nondirectory id)))
        (delete-file id)
        (message "Deleted %s" (file-name-nondirectory id))
        (hikizan-adk-ui-refresh)))
     ((bufferp id)
      (message "Selected item is a buffer. Use 'k' to kill its process."))
     (t
      (message "No session selected.")))))

(defun hikizan-adk-ui-open-dashboard (agent-path)
  "Open ADK sessions dashboard for AGENT-PATH."
  (let ((buffer (get-buffer-create "*hikizan-adk-sessions*")))
    (with-current-buffer buffer
      (hikizan-adk-ui-mode)
      (setq hikizan-adk--dashboard-agent-path (expand-file-name agent-path))
      (hikizan-adk-ui-refresh)
      (pop-to-buffer buffer))))

(defun hikizan-adk--resume-from-file (file-path)
  "Resume an ADK session from FILE-PATH."
  (let ((agent-path hikizan-adk--dashboard-agent-path))
    (hikizan-adk--run-process agent-path (list "--resume" file-path))))

(defun hikizan-adk-ui-open-file ()
  "Open the session file at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (stringp id)
        (find-file id)
      (message "Not a saved session file."))))

(defun hikizan-adk-ui--plan-exists-p (session-id)
  "Check if a plan file exists for SESSION-ID."
  (let* ((clean-id (if (and session-id (string-match "\\.session$" session-id))
                       (replace-match "" t t session-id)
                     session-id))
         (task-dir (expand-file-name "~/.emacs.d/python/emacs_agent/plans/"))
         (pattern (concat (regexp-quote clean-id) ".*\\.org$")))
    (and clean-id
         (file-directory-p task-dir)
         (directory-files task-dir nil pattern))))

(defun hikizan-adk-ui-open-planned-task ()
  "Open the planned task Org-Mode file for the current session."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (raw-session-id (cond
                          ((stringp id) (file-name-base id))
                          ((bufferp id) (with-current-buffer id hikizan-adk--session-id))))
         (session-id (if (and raw-session-id (string-match "\\.session$" raw-session-id))
                         (replace-match "" t t raw-session-id)
                       raw-session-id))
         (task-dir (expand-file-name "~/.emacs.d/python/emacs_agent/plans/")))
    (if (and session-id (file-directory-p task-dir))
        (let* ((pattern (concat (regexp-quote session-id) ".*\\.org$"))
               (files (directory-files task-dir t pattern)))
          (if files
              (dolist (file files)
                (let ((buf (find-file-noselect file)))
                  (with-current-buffer buf
                    (read-only-mode 1)
                    (auto-revert-mode 1))
                  (pop-to-buffer buf)))
            (message "No task plans found for session: %s" session-id)))
      (message "Session ID not found or task directory does not exist."))))

(provide 'hikizan-adk-ui)
;;; hikizan-adk-ui.el ends here
