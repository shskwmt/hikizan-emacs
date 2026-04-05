;;; hikizan-adk-core.el --- Core commands for ADK -*- lexical-binding: t; -*-

(require 'hikizan-adk-process)
(require 'hikizan-adk-ui)

;;;###autoload
(defun hikizan/adk-run (agent-path)
  "Start interactive adk run in AGENT-PATH.
This will always start a new session."
  (interactive (list (read-directory-name "Agent directory: ")))
  (hikizan/adk--run-process agent-path nil t))

;;;###autoload
(defun hikizan/adk-resume (agent-path session-file)
  "Start adk run --resume SESSION-FILE in AGENT-PATH."
  (interactive
   (let* ((dir (read-directory-name "Agent directory: "))
          (file (read-file-name "Session file: " dir nil t nil (lambda (f) (or (file-directory-p f) (string-match-p "\\.session\\.json$" f))))))
     (list dir file)))
  (hikizan/adk--run-process agent-path (list "--resume" session-file)))

;;;###autoload
(defun hikizan/adk-replay (agent-path session-file)
  "Start adk run --replay SESSION-FILE in AGENT-PATH (non-interactive)."
  (interactive
   (let* ((dir (read-directory-name "Agent directory: "))
          (file (read-file-name "Session file: " dir nil t nil (lambda (f) (or (file-directory-p f) (string-match-p "\\.session\\.json$" f))))))
     (list dir file)))
  (hikizan/adk--run-process agent-path (list "--replay" session-file))
  (let ((buf (get-buffer (format "*hikizan-adk:%s*" (file-name-nondirectory (directory-file-name agent-path))))))
    (when buf
      (with-current-buffer buf
        (setq-local buffer-read-only t)))))

;;;###autoload
(defun hikizan/adk-sessions (agent-path)
  "Open ADK sessions dashboard for AGENT-PATH."
  (interactive (list (read-directory-name "Agent directory: ")))
  (hikizan/adk-ui-open-dashboard agent-path))

(provide 'hikizan-adk-core)
