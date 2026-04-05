;;; hikizan-adk-ui.el --- Dashboard UI for ADK -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'hikizan-adk-process)

(defvar-local hikizan-adk--dashboard-agent-path nil)

(define-derived-mode hikizan-adk-ui-mode tabulated-list-mode "Hikizan-ADK-Dashboard"
  "Major mode for ADK sessions dashboard."
  (setq tabulated-list-format [("State" 10 t)
                               ("Name" 30 t)
                               ("PID" 10 t)
                               ("Updated" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'hikizan/adk-ui--refresh-entries nil t)
  (tabulated-list-init-header))

(define-key hikizan-adk-ui-mode-map (kbd "RET") #'hikizan/adk-ui-action)
(define-key hikizan-adk-ui-mode-map (kbd "g") #'hikizan/adk-ui-refresh)
(define-key hikizan-adk-ui-mode-map (kbd "k") #'hikizan/adk-ui-kill-session)

(defun hikizan/adk-ui--refresh-entries ()
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
                   (state (if live "running" "stopped"))
                   (pid (if live (format "%d" (process-id proc)) "-")))
              (push (list buf (vector state (buffer-name buf) pid "")) entries)))))

      ;; 2. Saved session files
      (when (file-directory-p agent-path)
        (let* ((files (directory-files agent-path t "\\.session\\.json$")))
	  (message "%s" files)
          (dolist (file files)
            (let* ((name (file-name-nondirectory file))
                   (attrs (file-attributes file))
                   (mtime (nth 5 attrs))
                   (updated (format-time-string "%Y-%m-%d %H:%M" mtime)))
              (push (list file (vector "saved" name "-" updated)) entries))))))
    (setq tabulated-list-entries entries)))

(defun hikizan/adk-ui-refresh ()
  "Refresh the dashboard."
  (interactive)
  (revert-buffer))

(defun hikizan/adk-ui-action ()
  "Perform action on current session."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((bufferp id)
      (pop-to-buffer id))
     ((stringp id)
      (hikizan/adk--resume-from-file id))
     (t (message "No action for this row.")))))

(defun hikizan/adk-ui-kill-session ()
  "Kill the process of the selected running session."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (bufferp id)
        (with-current-buffer id
          (hikizan/adk-kill)
          (hikizan/adk-ui-refresh))
      (message "Not a running session buffer."))))

(defun hikizan/adk-ui-open-dashboard (agent-path)
  "Open ADK sessions dashboard for AGENT-PATH."
  (let ((buffer (get-buffer-create "*hikizan-adk-sessions*")))
    (with-current-buffer buffer
      (hikizan-adk-ui-mode)
      (setq hikizan-adk--dashboard-agent-path (expand-file-name agent-path))
      (hikizan/adk-ui-refresh)
      (pop-to-buffer buffer))))

(defun hikizan/adk--resume-from-file (file-path)
  "Resume an ADK session from FILE-PATH."
  (let ((agent-path hikizan-adk--dashboard-agent-path))
    (hikizan/adk--run-process agent-path (list "--resume" file-path))))

(provide 'hikizan-adk-ui)
