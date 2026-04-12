;;; hikizan-adk-core.el --- Core commands for ADK -*- lexical-binding: t; -*-

;;; Commentary:
;; Core commands for ADK

;;; Code:

(require 'hikizan-adk-process)
(require 'hikizan-adk-ui)

;;;###autoload
(defun hikizan-adk-run (agent-path)
  "Start interactive adk run in AGENT-PATH.
This will always start a new session."
  (interactive (list (read-directory-name "Agent directory: ")))
  (hikizan-adk--run-process agent-path nil t))

;;;###autoload
(defun hikizan-adk-sessions (agent-path &optional sessions-path)
  "Open ADK sessions dashboard for AGENT-PATH.
If SESSIONS-PATH is provided, use it as the base directory for sessions."
  (interactive (list (read-directory-name "Agent directory: ")))
  (hikizan-adk-ui-open-dashboard agent-path sessions-path))

(provide 'hikizan-adk-core)
;;; hikizan-adk-core.el ends here
