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
      (when-let* ((process (map-elt client :process))
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

(ert-deftest acp-test-session-list-request-omits-cursor-by-default ()
  "Request the first page when no cursor is given."
  (should (equal (acp-make-session-list-request :cwd "/tmp/")
                 '((:method . "session/list")
                   (:params . ((cwd . "/tmp")))))))

(ert-deftest acp-test-session-list-request-includes-cursor ()
  "Carry an opaque cursor into the next page request."
  (should (equal (acp-make-session-list-request :cwd "/tmp/" :cursor "page-2")
                 '((:method . "session/list")
                   (:params . ((cwd . "/tmp")
                               (cursor . "page-2")))))))

(ert-deftest acp-test-session-list-request-requires-cwd ()
  "Refuse to build a request without a cwd."
  (should-error (acp-make-session-list-request :cursor "page-2")))

(defun acp-test--captured-filter (client)
  "Start CLIENT with a stubbed process and return its `:filter'."
  (let (filter)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq filter (plist-get args :filter))
                 nil)))
      (acp--start-client :client client))
    filter))

(defun acp-test--notification-line (n)
  "Return a newline-terminated notification carrying number N."
  (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"n\":%d}}\n" n))

(defun acp-test--drain-until (predicate &optional timeout)
  "Return non-nil once PREDICATE does, running timers for up to TIMEOUT seconds.

TIMEOUT defaults to 1.  Returns nil if PREDICATE never held."
  (let ((deadline (+ (float-time) (or timeout 1))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      ;; The drain reschedules itself, so keep pumping rather than
      ;; assuming a single timer runs everything.  A quitting handler
      ;; signals through `accept-process-output', so contain it here.
      (condition-case nil
          (accept-process-output nil 0.02)
        (quit nil)))
    (funcall predicate)))

(ert-deftest acp-test-drain-survives-quitting-notification-handler ()
  "Keep routing queued messages after a handler exits non-locally."
  (let* ((acp-logging-enabled nil)
         (client (acp-make-client :command "cat"))
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (let ((n (map-nested-elt notification '(params n))))
                          (push n received)
                          ;; Stand in for C-g arriving mid-drain.
                          (when (= n 2)
                            (signal 'quit nil)))))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (funcall filter nil (mapconcat #'acp-test--notification-line '(1 2 3 4) ""))
          (acp-test--drain-until (lambda () (= (length received) 4)))
          ;; 3 and 4 are queued behind the quitting handler: without a
          ;; rescheduled drain they are never routed.
          (should (equal (nreverse received) '(1 2 3 4))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-unwedges-queue-after-quitting-handler ()
  "Route messages arriving after a handler exited non-locally."
  (let* ((acp-logging-enabled nil)
         (client (acp-make-client :command "cat"))
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (let ((n (map-nested-elt notification '(params n))))
                          (push n received)
                          (when (= n 1)
                            (signal 'quit nil)))))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (funcall filter nil (acp-test--notification-line 1))
          (acp-test--drain-until (lambda () (= (length received) 1)))
          ;; A stuck busy flag would swallow every later message.
          (funcall filter nil (acp-test--notification-line 2))
          (acp-test--drain-until (lambda () (= (length received) 2)))
          (should (equal (nreverse received) '(1 2))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-survives-repeated-quitting-handler ()
  "Drain the whole queue when every handler exits non-locally."
  (let* ((acp-logging-enabled nil)
         (client (acp-make-client :command "cat"))
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push (map-nested-elt notification '(params n)) received)
                        (signal 'quit nil)))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (funcall filter nil (mapconcat #'acp-test--notification-line '(1 2 3 4 5) ""))
          ;; Each exit drops its own message before rescheduling, so the
          ;; queue shrinks instead of retrying the same message forever.
          (acp-test--drain-until (lambda () (= (length received) 5)))
          (should (equal (nreverse received) '(1 2 3 4 5))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-survives-unroutable-message ()
  "Keep routing after a message that routing itself cannot handle.

A JSON line that parses to something other than an object signals from
the drain rather than from a handler, so no `condition-case' contains
it."
  (let* ((acp-logging-enabled nil)
         (client (acp-make-client :command "cat"))
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push (map-nested-elt notification '(params n)) received)))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          ;; Queued behind the unroutable message, 1 needs a rescheduled
          ;; drain to reach a handler at all.
          (funcall filter nil (concat "[1,2]\n" (acp-test--notification-line 1)))
          ;; Routing the unroutable message signals, and the timer
          ;; reports that.  Expected here, so keep it out of the logs.
          (let ((inhibit-message t)
                (message-log-max nil))
            (acp-test--drain-until (lambda () (= (length received) 1))))
          ;; And 2 needs the busy flag to have been cleared.
          (funcall filter nil (acp-test--notification-line 2))
          (acp-test--drain-until (lambda () (= (length received) 2)))
          (should (equal (nreverse received) '(1 2))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-yields-between-batches ()
  "Stop a drain once its time budget is spent, resuming on the next one.

A backlog that outlasts the budget must be routed across several
callbacks rather than one, so Emacs gets a chance to redisplay and read
input in between."
  (let* ((acp-logging-enabled nil)
         (acp-drain-time-budget 0.01)
         (client (acp-make-client :command "cat"))
         (drains 0)
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        ;; Outlasts the budget, so the second message of
                        ;; any batch is already over it.
                        (sleep-for 0.02)
                        (push (map-nested-elt notification '(params n)) received)))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (advice-add 'timer-event-handler :before
                      (lambda (&rest _) (setq drains (1+ drains)))
                      '((name . acp-test-count-drains)))
          (unwind-protect
              (progn
                (funcall filter nil (mapconcat #'acp-test--notification-line
                                               '(1 2 3 4) ""))
                (should (acp-test--drain-until
                         (lambda () (= (length received) 4)) 3)))
            (advice-remove 'timer-event-handler 'acp-test-count-drains))
          ;; Order is preserved and nothing is dropped.
          (should (equal (nreverse received) '(1 2 3 4)))
          ;; One callback per message: the budget is spent by the first
          ;; handler, so each batch routes one and reschedules.  A single
          ;; drain for the whole backlog would be the unbatched behavior.
          (should (>= drains 4)))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-routes-one-message-per-batch-minimum ()
  "Route at least one message per drain however small the budget is.

Testing the budget after routing (not before) is what keeps a zero
budget from spinning on a queue it never advances."
  (let* ((acp-logging-enabled nil)
         (acp-drain-time-budget 0)
         (client (acp-make-client :command "cat"))
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push (map-nested-elt notification '(params n)) received)))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (funcall filter nil (mapconcat #'acp-test--notification-line
                                         '(1 2 3) ""))
          (should (acp-test--drain-until (lambda () (= (length received) 3)) 3))
          (should (equal (nreverse received) '(1 2 3))))
      (acp-shutdown :client client))))

(ert-deftest acp-test-drain-without-budget-empties-queue-in-one-batch ()
  "Drain the whole queue in a single callback when the budget is nil."
  (let* ((acp-logging-enabled nil)
         (acp-drain-time-budget nil)
         (client (acp-make-client :command "cat"))
         (batch-sizes nil)
         (routed 0)
         received filter)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq routed (1+ routed))
                        (push (map-nested-elt notification '(params n)) received)))
    (unwind-protect
        (progn
          (setq filter (acp-test--captured-filter client))
          (advice-add 'timer-event-handler :around
                      (lambda (orig &rest args)
                        (let ((before routed))
                          (apply orig args)
                          (when (> routed before)
                            (push (- routed before) batch-sizes))))
                      '((name . acp-test-batch-sizes)))
          (unwind-protect
              (progn
                (funcall filter nil (mapconcat #'acp-test--notification-line
                                               '(1 2 3 4 5) ""))
                (should (acp-test--drain-until
                         (lambda () (= (length received) 5)) 3)))
            (advice-remove 'timer-event-handler 'acp-test-batch-sizes))
          (should (equal (nreverse received) '(1 2 3 4 5)))
          (should (equal batch-sizes '(5))))
      (acp-shutdown :client client))))

(provide 'acp-test)

;;; acp-test.el ends here
