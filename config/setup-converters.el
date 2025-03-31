(defun memacs/unix-timestamp-to-iso (timestamp)
  "Convert a Unix timestamp (in milliseconds) to ISO 8601 format in GMT+1 timezone."
  (interactive "nEnter Unix timestamp (milliseconds): ")
  (let* ((time (seconds-to-time (/ timestamp 1000))) ;; Convert ms to seconds
         (iso-date (format-time-string "%Y-%m-%dT%H:%M:%S%z" time)))
    (message "ISO 8601 Date (GMT+1): %s" iso-date)
    iso-date))

(use-package html-to-hiccup)

(provide 'setup-converters)
