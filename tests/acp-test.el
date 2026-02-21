;;; acp-test.el --- Tests for acp.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for ACP log buffer trimming behavior.
;;
;; The trimming logic should enforce byte limits while preserving whole
;; log messages using boundary markers.

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'acp)

(defun acp-test--format-log-message (message)
  "Return a formatted log message for MESSAGE."
  (acp--format-log-message (car message) "%s" (cdr message)))

(defun acp-test-log-buffer-string (max-bytes &rest messages)
  "Log MESSAGES with MAX-BYTES and return the log buffer contents."
  (let* ((acp-logging-enabled t)
         (acp--log-buffer-max-bytes max-bytes)
         (client (list (cons :command (make-temp-name "acp-test-"))
                       (cons :instance-count 1)))
         (log-buffer (acp-logs-buffer :client client)))
    (unwind-protect
        (with-current-buffer log-buffer
          (erase-buffer)
          (dolist (message messages)
            (acp--log client (car message) "%s" (cdr message)))
          (buffer-string))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer)))))

(ert-deftest acp-test-trim-log-buffer-unibyte ()
  "Trim unibyte logs on whole-message boundaries."
  (let* ((msg1 (cons "A" "one"))
         (msg2 (cons "B" "two"))
         (msg3 (cons "C" "three"))
         (log1 (acp-test--format-log-message msg1))
         (log2 (acp-test--format-log-message msg2))
         (log3 (acp-test--format-log-message msg3))
         (max-bytes (+ (string-bytes log2) (string-bytes log3)))
         (messages (list msg1 msg2 msg3))
         (result (apply #'acp-test-log-buffer-string max-bytes messages)))
    (should (equal result (concat log2 log3)))
    (should (<= (string-bytes result) max-bytes))))

(ert-deftest acp-test-trim-log-buffer-multibyte ()
  "Trim multibyte logs by bytes while keeping whole messages."
  (let* ((msg1 (cons "A" "alpha"))
         (msg2 (cons "B" "café ✓"))
         (msg3 (cons "C" "omega"))
         (log1 (acp-test--format-log-message msg1))
         (log2 (acp-test--format-log-message msg2))
         (log3 (acp-test--format-log-message msg3))
         (chars-m2m3 (+ (length log2) (length log3)))
         (bytes-m2m3 (+ (string-bytes log2) (string-bytes log3)))
         (max-bytes (1+ chars-m2m3))
         (messages (list msg1 msg2 msg3))
         (result (apply #'acp-test-log-buffer-string max-bytes messages)))
    (should (< max-bytes bytes-m2m3))
    (should (equal result log3))
    (should (<= (string-bytes result) max-bytes))))

(provide 'acp-test)

;;; acp-test.el ends here
