;;; hikizan-adk-telemetry.el --- Telemetry viewer for ADK -*- lexical-binding: t; -*-

;;; Commentary:
;; Telemetry viewer for ADK using JSONL.
;; Strategy: Load file into a buffer, index metadata, and use string search for details.
;; This avoids complex byte-offset math and encoding issues.

;;; Code:

(require 'json)
(require 'tabulated-list)
(require 'cl-lib)

(cl-defstruct hikizan-adk-span
  trace-id span-id parent-id name timestamp)

(defvar-local hikizan-adk-telemetry--index nil
  "In-memory index of spans grouped by Trace ID.")

(defvar-local hikizan-adk-telemetry--data-buffer nil
  "The buffer containing the raw telemetry data.")

(defvar-local hikizan-adk-telemetry--view 'trace
  "Current view level: 'trace or 'span.")

(defvar-local hikizan-adk-telemetry--selected-trace nil
  "The Trace ID currently being inspected.")

(define-derived-mode hikizan-adk-telemetry-mode tabulated-list-mode "ADK-Telemetry"
  "Major mode for viewing ADK telemetry."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'hikizan-adk-telemetry-revert nil t))

(define-key hikizan-adk-telemetry-mode-map (kbd "RET") #'hikizan-adk-telemetry-action)
(define-key hikizan-adk-telemetry-mode-map (kbd "g") #'hikizan-adk-telemetry-refresh)
(define-key hikizan-adk-telemetry-mode-map (kbd "b") #'hikizan-adk-telemetry-back)
(define-key hikizan-adk-telemetry-mode-map (kbd "u") #'hikizan-adk-telemetry-back)

(defun hikizan-adk-telemetry--get-val (key alist)
  "Get value for KEY in ALIST, supporting both symbol and string keys."
  (or (cdr (assoc key alist))
      (cdr (assoc (symbol-name key) alist))
      (when (stringp key) (cdr (assoc (intern key) alist)))))

;;;###autoload
(defun hikizan-adk-telemetry-open (file)
  "Open a telemetry JSONL file for viewing."
  (interactive "fTelemetry file (JSONL): ")
  (let* ((file-abs (expand-file-name file))
         (data-buf (find-file-noselect file-abs))
         (view-buf (get-buffer-create (format "*adk-telemetry:%s*" (file-name-nondirectory file)))))
    (with-current-buffer view-buf
      (hikizan-adk-telemetry-mode)
      (setq hikizan-adk-telemetry--data-buffer data-buf)
      (setq hikizan-adk-telemetry--view 'trace)
      (setq hikizan-adk-telemetry--index nil)
      (hikizan-adk-telemetry-revert)
      (pop-to-buffer view-buf))))

(defun hikizan-adk-telemetry-refresh ()
  "Refresh the telemetry data and current view."
  (interactive)
  (when (buffer-live-p hikizan-adk-telemetry--data-buffer)
    (with-current-buffer hikizan-adk-telemetry--data-buffer
      (revert-buffer t t)))
  (setq hikizan-adk-telemetry--index nil)
  (hikizan-adk-telemetry-revert))

(defun hikizan-adk-telemetry-revert ()
  "Refresh the display based on current view and index."
  (interactive)
  (unless (buffer-live-p hikizan-adk-telemetry--data-buffer)
    (error "Data buffer no longer exists"))
  (unless hikizan-adk-telemetry--index
    (setq hikizan-adk-telemetry--index (hikizan-adk-telemetry--index-buffer hikizan-adk-telemetry--data-buffer)))
  (if (eq hikizan-adk-telemetry--view 'trace)
      (hikizan-adk-telemetry--show-traces)
    (hikizan-adk-telemetry--show-spans hikizan-adk-telemetry--selected-trace)))

(defun hikizan-adk-telemetry--show-traces ()
  (setq tabulated-list-format [("Start Time" 24 t)
                               ("Trace ID" 20 t)
                               ("Spans" 8 t)
                               ("Root Span" 40 t)])
  (tabulated-list-init-header)
  (let (entries)
    (dolist (trace hikizan-adk-telemetry--index)
      (let* ((trace-id (car trace))
             (spans (cdr trace))
             (root-span (car spans))
             (count (length spans)))
        (push (list trace-id
                    (vector (hikizan-adk-span-timestamp root-span)
                            trace-id
                            (number-to-string count)
                            (hikizan-adk-span-name root-span)))
              entries)))
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-print t))

(defun hikizan-adk-telemetry--show-spans (trace-id)
  (setq tabulated-list-format [("Timestamp" 24 t)
                               ("Span ID" 20 t)
                               ("Name" 40 t)])
  (tabulated-list-init-header)
  (let* ((spans (cdr (assoc trace-id hikizan-adk-telemetry--index)))
         (entries (mapcar (lambda (span)
                            (list span
                                  (vector (hikizan-adk-span-timestamp span)
                                          (hikizan-adk-span-span-id span)
                                          (hikizan-adk-span-name span))))
                          spans)))
    (setq tabulated-list-entries entries))
  (tabulated-list-print t))

(defun hikizan-adk-telemetry-action ()
  "Handle selection in the telemetry viewer."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((eq hikizan-adk-telemetry--view 'trace)
      (setq hikizan-adk-telemetry--view 'span)
      (setq hikizan-adk-telemetry--selected-trace id)
      (hikizan-adk-telemetry-revert))
     ((hikizan-adk-span-p id)
      (hikizan-adk-telemetry-show-detail id)))))

(defun hikizan-adk-telemetry-back ()
  (interactive)
  (when (eq hikizan-adk-telemetry--view 'span)
    (setq hikizan-adk-telemetry--view 'trace)
    (setq hikizan-adk-telemetry--selected-trace nil)
    (hikizan-adk-telemetry-revert)))

(defun hikizan-adk-telemetry--index-buffer (buffer)
  "Scan BUFFER and create an index of spans grouped by Trace ID."
  (let ((traces (make-hash-table :test 'equal))
        (trace-order nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (condition-case nil
                  (let* ((data (json-read-from-string line))
                         (context (hikizan-adk-telemetry--get-val 'context data))
                         (trace-id (or (hikizan-adk-telemetry--get-val 'trace_id context)
                                       (hikizan-adk-telemetry--get-val 'traceId data)
                                       "unknown"))
                         (span-id (or (hikizan-adk-telemetry--get-val 'span_id context)
                                      (hikizan-adk-telemetry--get-val 'spanId data)
                                      ""))
                         (parent-id (or (hikizan-adk-telemetry--get-val 'parent_id data)
                                        (hikizan-adk-telemetry--get-val 'parentSpanId data)))
                         (timestamp (or (hikizan-adk-telemetry--get-val 'start_time data)
                                        (hikizan-adk-telemetry--get-val 'startTimeUnixNano data)
                                        ""))
                         (name (or (hikizan-adk-telemetry--get-val 'name data) ""))
                         (span (make-hikizan-adk-span
                                :trace-id trace-id
                                :span-id span-id
                                :parent-id parent-id
                                :name name
                                :timestamp timestamp)))
                    (unless (gethash trace-id traces)
                      (setq trace-order (append trace-order (list trace-id))))
                    (puthash trace-id (cons span (gethash trace-id traces)) traces))
                (error nil))))
          (forward-line 1))))
    (mapcar (lambda (tid)
              (cons tid (nreverse (gethash tid traces))))
            trace-order)))

(defun hikizan-adk-telemetry-show-detail (span)
  "Show full details of SPAN by searching for its ID in the data buffer."
  (let* ((span-id (hikizan-adk-span-span-id span))
         (data-buf hikizan-adk-telemetry--data-buffer)
         (raw-text (with-current-buffer data-buf
                     (save-excursion
                       (goto-char (point-min))
                       ;; Search for the exact span-id in the JSON content
                       (if (re-search-forward (format "\"span_?id\"\\s-*:\\s-*\"%s\"" span-id) nil t)
                           (buffer-substring (line-beginning-position) (line-end-position))
                         (error "Could not find span %s in data buffer" span-id)))))
         (buf (get-buffer-create "*adk-telemetry-detail*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert raw-text)
        (if (fboundp 'js-json-mode) (js-json-mode) (javascript-mode))
        (ignore-errors (json-pretty-print-buffer)))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (pop-to-buffer buf))))

(provide 'hikizan-adk-telemetry)
;;; hikizan-adk-telemetry.el ends here
