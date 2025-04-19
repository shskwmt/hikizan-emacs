;;; hikizan-util.el --- util -*- lexical-binding: t; -*-

(defun hikizan/extract-buffer-or-active-region-string ()
  "Extract the buffer or the active region string."
  (let ((content (if (region-active-p)
		    (buffer-substring-no-properties
		     (region-beginning)
		     (region-end))
		  (buffer-substring-no-properties
		   (point-min)
		   (point-max)))))
    content))

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

(defun hikizan/grep (pattern file-pattern path)
  "Execute grep and return the result string."
  (with-temp-buffer
    (call-process "rg" nil (current-buffer) nil "-nHS" "-e" pattern "-g" file-pattern path)
    (buffer-string)))

(defun hikizan/grep-files (pattern file-pattern path)
  "Execute grep and return the result string."
  (with-temp-buffer
    (call-process "rg" nil (current-buffer) nil "-lS" "-e" pattern "-g" file-pattern path)
    (buffer-string)))

(defun hikizan/eval-elisp-file (file-path)
  "Evaluate the Elisp code in the specified FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (print file-path)
    (condition-case err
	(eval-buffer)
      (error (message "error: %s" (error-message-string err))))))

(defun hikizan/safe-elisp-eval (elisp)
  "Safely evaluate ELISP with error handling."
  (condition-case err
      (format "Evaluation Results:\n%s" (eval (read elisp)))
    (error (format "Error: %s" err))))

(provide 'hikizan-util)
