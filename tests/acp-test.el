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

(ert-deftest acp-test-stderr-preserves-whitespace-only-chunks ()
  "Forward stderr chunks unchanged so consumers can concatenate them."
  (let ((client (acp-make-client :command "cat"))
        (acp-logging-enabled nil)
        (chunks '("hello" " " "world" "\n" "\t" "next\n"))
        stderr-buffer received)
    (acp-subscribe-to-errors
     :client client
     :on-error (lambda (error-data)
                 (should (= (map-elt error-data 'code) -32603))
                 (push (map-elt error-data 'message) received)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq stderr-buffer (plist-get args :stderr))
                       nil)))
            (acp--start-client :client client))
          (with-current-buffer stderr-buffer
            (dolist (chunk chunks)
              (insert chunk)))
          (should (equal (nreverse received) chunks)))
      (when (buffer-live-p stderr-buffer)
        (kill-buffer stderr-buffer)))))

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

(ert-deftest acp-test-initialize-advertises-form-elicitation ()
  "Serialize form elicitation as an empty object, not JSON null."
  (let* ((request (acp-make-initialize-request
                   :protocol-version 1
                   :elicitation-form-capability t))
         (form (map-nested-elt
                request '(:params clientCapabilities elicitation form)))
         (json (acp--serialize-json request)))
    (should (hash-table-p form))
    (should (equal 0 (hash-table-count form)))
    (should (string-match-p
             (rx "\"elicitation\":" (* space)
                 "{" (* space)
                 "\"form\":" (* space) "{}" (* space)
                 "}")
             json))
    (should-not (string-match-p "\"form\":null" json))))

(ert-deftest acp-test-initialize-omits-form-elicitation-by-default ()
  "Do not promise support for form elicitation unless requested."
  (let ((request (acp-make-initialize-request :protocol-version 1)))
    (should-not
     (map-contains-key
      (map-nested-elt request '(:params clientCapabilities))
      'elicitation))))

(ert-deftest acp-test-make-elicitation-response ()
  "Construct each valid elicitation response shape."
  (should
   (equal (acp-make-elicitation-response
           :request-id 7
           :action "accept"
           :content '((authorized . t)))
          '((:request-id . 7)
            (:result . ((action . "accept")
                        (content . ((authorized . t))))))))
  (should
   (equal (acp-make-elicitation-response
           :request-id "request-8"
           :action "decline")
          '((:request-id . "request-8")
            (:result . ((action . "decline"))))))
  (should
   (equal (acp-make-elicitation-response
           :request-id 9
           :action "cancel")
          '((:request-id . 9)
            (:result . ((action . "cancel"))))))
  (should-error
   (acp-make-elicitation-response
    :request-id 10
    :action "decline"
    :content '((answer . "no")))))

(ert-deftest acp-test-request-reports-id-before-transmission ()
  "Report a request's wire id after registration and before writing it."
  (let ((client (acp-make-client :command "cat"))
        events)
    (unwind-protect
        (progn
          (acp--start-client :client client)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (push 'written events))))
            (acp-send-request
             :client client
             :request '((:method . "initialize"))
             :on-sent
             (lambda (event)
               (should
                (map-nested-elt
                 client
                 `(:pending-requests
                   ,(map-elt event :request-id))))
               (push 'registered events))))
          (should (equal events '(written registered))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-request-observer-failure-aborts-send ()
  "Do not transmit or retain a request rejected by its ON-SENT observer."
  (let ((client (acp-make-client :command "cat"))
        written)
    (unwind-protect
        (progn
          (acp--start-client :client client)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (setq written t))))
            (should-error
             (acp-send-request
              :client client
              :request '((:method . "initialize"))
              :on-sent (lambda (_event)
                         (error "Reject request")))))
          (should-not written)
          (should-not (map-elt client :pending-requests)))
      (acp-shutdown :client client))))

(ert-deftest acp-test-request-transmission-failure-rolls-back-registration ()
  "Do not retain a request when its process write fails."
  (let ((client (acp-make-client :command "cat")))
    (unwind-protect
        (progn
          (acp--start-client :client client)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (error "Write failed"))))
            (should-error
             (acp-send-request
              :client client
              :request '((:method . "initialize")))))
          (should-not (map-elt client :pending-requests)))
      (acp-shutdown :client client))))

(ert-deftest acp-test-process-exit-notifies-once ()
  "Notify process-exit subscribers once for an independently exiting client."
  (let ((client (acp-make-client :command "cat"))
        events)
    (acp-subscribe-to-process-exits
     :client client
     :on-exit (lambda (event)
                (push event events)))
    (acp--start-client :client client)
    (delete-process (map-elt client :process))
    (while (process-live-p (map-elt client :process))
      (accept-process-output nil 0.05))
    (accept-process-output nil 0.05)
    (should (= (length events) 1))
    (should (stringp (map-elt (seq-first events) :event)))
    (acp-shutdown :client client)
    (should (= (length events) 1))))

(ert-deftest acp-test-stale-process-sentinel-does-not-end-replacement ()
  "Finish an old generation without letting its sentinel end its replacement."
  (let ((client (acp-make-client :command "cat"))
        events
        failed)
    (unwind-protect
        (progn
          (acp-subscribe-to-process-exits
           :client client
           :on-exit (lambda (event)
                      (push event events)))
          (acp--start-client :client client)
          (let* ((old-process (map-elt client :process))
                 (old-sentinel (process-sentinel old-process)))
            ;; Delay the real sentinel until after a replacement starts.
            (set-process-sentinel old-process #'ignore)
            (delete-process old-process)
            (map-put! client :pending-requests
                      `((76 . ((:on-failure
                               . ,(lambda (_error)
                                    (setq failed t)))))))
            (acp--start-client :client client)
            (let ((replacement (map-elt client :process))
                  (pending '((77 . ((:request . request))))))
              (should failed)
              (should (= (length events) 1))
              (map-put! client :pending-requests pending)
              (funcall old-sentinel old-process "finished\n")
              (should (eq replacement (map-elt client :process)))
              (should (equal pending
                             (map-elt client :pending-requests)))
              (should (= (length events) 1)))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-shutdown-notifies-process-exit-subscribers ()
  "Notify subscribers before intentional shutdown releases them."
  (let ((client (acp-make-client :command "cat"))
        event)
    (acp--start-client :client client)
    (acp-subscribe-to-process-exits
     :client client
     :on-exit (lambda (value)
                (setq event value)))
    (acp-shutdown :client client)
    (should (equal (map-elt event :event) "shutdown"))))

(ert-deftest acp-test-sync-request-fails-when-agent-exits-after-read ()
  "Synchronous requests error instead of waiting forever after agent exit."
  (let ((client (acp-make-client
                 :command "sh"
                 :command-params '("-c" "IFS= read -r _; exit 42"))))
    (unwind-protect
        (should-error
         (acp-send-request
          :client client
          :request '((:method . "initialize"))
          :sync t))
      (when-let ((process (map-elt client :process))
                 ((process-live-p process)))
        (delete-process process)))))

(defun acp-test--exited-client ()
  "Return a started client whose process has since exited."
  (let* ((client (acp-make-client :command "cat"))
         (process (progn (acp--start-client :client client)
                         (map-elt client :process))))
    (delete-process process)
    (while (process-live-p process)
      (accept-process-output nil 0.05))
    client))

(ert-deftest acp-test-shutdown-releases-client-whose-process-exited ()
  "Release handlers and buffers even when the process already exited."
  (let ((client (acp-test--exited-client)))
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (should (map-elt client :notification-handlers))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-releases-running-client ()
  "Release handlers and buffers for a client with a live process."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-is-idempotent ()
  "Leave no buffers behind when shutdown is called more than once."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-logs-buffer :client client)
    (acp-traffic-buffer :client client)
    (acp-shutdown :client client)
    (acp-shutdown :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (should-not (get-buffer (acp--traffic-buffer-name client)))))

(ert-deftest acp-test-shutdown-does-not-create-buffers ()
  "Do not resurrect buffers that were never opened."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (acp-shutdown :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (should-not (get-buffer (acp--traffic-buffer-name client)))))

(ert-deftest acp-test-shutdown-tolerates-externally-killed-buffer ()
  "Release the traffic buffer when the log buffer is already gone."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (kill-buffer (acp-logs-buffer :client client))
    (let ((traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-releases-never-started-client ()
  "Release handlers on a client that was never started."
  (let ((client (acp-make-client :command "cat")))
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (acp-shutdown :client client)
    (should-not (map-elt client :notification-handlers))))

(ert-deftest acp-test-shutdown-after-restart-releases-again ()
  "Allow a restarted client to be shut down a second time."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-shutdown :client client)
    (acp--start-client :client client)
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(provide 'acp-test)

;;; acp-test.el ends here
