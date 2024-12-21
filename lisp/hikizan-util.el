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

(provide 'hikizan-util)
