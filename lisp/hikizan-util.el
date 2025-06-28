;;; hikizan-util.el --- util -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions

;;; Code:

(require 'project)

(defun hikizan/find-string-position-in-buffer (buffer search-string)
  "Find the position of SEARCH-STRING in the specified BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward search-string nil t)
	  (point)
	nil))))

(defun hikizan/copy-buffer-file-name ()
  "Copy buffer file name."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
	(kill-new file-name))))

(defun hikizan/copy-buffer-file-relative-path ()
  "Copy buffer file relative path."
  (interactive)
  (let* ((project (project-current))
	 (project-root (if project (project-root project)))
	 (buffer-file (buffer-file-name))
	 (relative-path (if (and project-root buffer-file)
                            (file-relative-name buffer-file project-root))))
    (kill-new relative-path)))

(defun hikizan/get-string-from-point (buffer point)
  "Get the string from BUFFER starting at POINT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties point (point-max)))))

(defun hikizan/write-string-to-file (filename content)
  "Write CONTENT to FILENAME."
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) filename)))

(defun hikizan/eval-elisp-file (file-path)
  "Evaluate the Elisp code in the specified FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (print file-path)
    (condition-case err
	(eval-buffer)
      (error (message "error: %s" (error-message-string err))))))

(provide 'hikizan-util)
