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

(defgroup hikizan/reading-pacemaker nil
  "Automatically advance point to pace your reading."
  :group 'convenience)

(defcustom hikizan/reading-pacemaker-interval 0.4
  "Default interval, in seconds, between each word step."
  :type 'number
  :group 'hikizan/reading-pacemaker)

(defvar hikizan/reading-pacemaker-timer nil
  "Timer object for the reading pacemaker.")

(defun hikizan/reading-pacemaker--step ()
  "Move forward one word and recenter the window."
  (forward-word 1)
  (recenter))

(defun hikizan/reading-pacemaker-start (&optional interval)
  "Start the reading pacemarker.

Every INTERVAL seconds, move forward one word and recenter."
  (interactive
   (list (when current-prefix-arg
	   (read-number
	    (format "Interval between words (seconds) [%s]: " hikizan/reading-pacemaker-interval)
	    hikizan/reading-pacemaker-interval))))
  ;; if already running, cancel first
  (when (timerp reading-pacemaker-timer)
    (cancel-timer reading-pacemaker-timer))
  (setq reading-pacemaker-timer
	(run-with-timer 0 hikizan/reading-pacemaker-interval #'reading-pacemaker--step))
  (message "Reading pacemaker started: %s sec/word" hikizan/reading-pacemaker-interval))

(defun hikizan/reading-pacemaker-stop ()
  "Stop the reading pacemaker."
  (interactive)
  (when (timerp reading-pacemaker-timer)
    (cancel-timer reading-pacemaker-timer)
    (setq reading-pacemaker-timer nil)
    (message "Reading pacemaker stopped")))

(provide 'hikizan-util)
