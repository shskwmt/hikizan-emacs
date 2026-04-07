;;; hikizan-org.el --- org  -*- lexical-binding: t; -*-

(setq org-directory (or (getenv "EMACS_ORG_DIR") "~/org/"))
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-agenda-files (list org-directory))
(setq org-log-done 'time)
(setq org-capture-templates
      '(("t" "Todo" entry (file org-default-notes-file)
	 "* TODO %?\n  %T")
        ("m" "Memo" entry (file org-default-notes-file)
	 "* %?\n  %T")))

;;; Code:

(defun hikizan-open-org-notes ()
  "Open org notes file"
  (interactive)
  (find-file org-default-notes-file))

(provide 'hikizan-org)
;;; hikizan-org.el ends here
