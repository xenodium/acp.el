;;; acp-fakes.el --- A fake ACP client -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/acp.el

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; acp-fakes enable faking ACP infrastructure to allow integration
;; in isolation.
;;
;; A fake client replays a recorded traffic file the way the real client
;; consumes a live connection: one ordered stream, each message delivered
;; exactly once.
;;
;; The replay keeps a cursor over the recording and pumps it forward.
;; Incoming messages (responses, agent-initiated requests, notifications)
;; are routed as the cursor reaches them, through the same
;; `acp--route-incoming-message' the real process filter uses -- so a
;; response resolves its pending request and a notification lands in
;; whatever turn is active at that point in the stream.
;;
;; An outgoing message the client has not sent yet stops the pump: it is a
;; barrier standing for "the recording did something here that the driver
;; must do too".  Sending the matching request or response claims the
;; barrier and the pump resumes.  A driver that will never send it (say a
;; `session/list' the code under test does not issue) skips it with
;; `acp-fakes-skip-barrier'; the reply left behind is then unclaimed and
;; routes to nothing.
;;
;; Ordering falls out of the cursor rather than out of scanning the
;; recording for traffic "related" to a request.  Related-traffic windows
;; nest and overlap (a long-running request can span a whole pushed turn),
;; which made the same message reachable from several paths and left its
;; turn ambiguous.

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)

(defun acp-fakes-make-client (messages)
  "Create a fake ACP client that replays traffic MESSAGES.

Each message is of the form:

\((:direction . ...)
  (:kind . ...)
  (:object . ...))"
  (let ((client (acp-make-client
                 :command "cat"
                 :command-params nil
                 :environment-variables nil
                 :request-sender (cl-function (lambda (&key client request buffer on-success on-failure _sync)
                                                (acp-fakes--request-sender
                                                 :client client
                                                 :request request
                                                 :buffer buffer
                                                 :on-success on-success
                                                 :on-failure on-failure)))
                 :response-sender
                 (cl-function (lambda (&key client response)
                                (acp-fakes--response-sender :client client :response response)))
                 :notification-sender
                 (cl-function (lambda (&key client notification &allow-other-keys)
                                (acp-fakes--notification-sender
                                 :client client :notification notification))))))
    ;; `:request-resolver' is left at acp's own, so replayed responses
    ;; resolve through the same path a live connection uses.  That needs
    ;; pending requests kept in acp's shape, keyed by recorded id.
    (setf (map-elt client :message-queue) (copy-sequence messages))
    (setf (map-elt client :pending-requests) '())
    (setf (map-elt client :traffic) (vconcat messages))
    (setf (map-elt client :cursor) 0)
    (setf (map-elt client :claimed) (make-hash-table :test 'eql))
    (setf (map-elt client :pumping) nil)
    client))

(defun acp-fakes--traffic (client)
  "Return CLIENT's recorded traffic vector."
  (map-elt client :traffic))

(defun acp-fakes--claimed-p (client index)
  "Return non-nil when CLIENT's recorded message at INDEX was claimed."
  (gethash index (map-elt client :claimed)))

(defun acp-fakes--claim (client index)
  "Mark CLIENT's recorded message at INDEX as claimed."
  (puthash index t (map-elt client :claimed)))

(defun acp-fakes--claim-outgoing (client predicate)
  "Claim CLIENT's first unclaimed outgoing message satisfying PREDICATE.

Returns the claimed message, or nil when the recording holds no
counterpart -- a request the code under test makes but the capture never
did, which simply goes unanswered."
  (let ((traffic (acp-fakes--traffic client))
        (found nil))
    (dotimes (index (length traffic))
      (unless found
        (let ((message (aref traffic index)))
          (when (and (eq (map-elt message :direction) 'outgoing)
                     (not (acp-fakes--claimed-p client index))
                     (funcall predicate message))
            (acp-fakes--claim client index)
            (setq found message)))))
    found))

(defun acp-fakes--route (client message)
  "Route incoming MESSAGE to CLIENT as the live process filter would."
  (acp--route-incoming-message
   :message message
   :client client
   :on-notification
   (lambda (notification)
     (dolist (handler (map-elt client :notification-handlers))
       (funcall handler notification)))
   :on-request
   (lambda (request)
     (dolist (handler (map-elt client :request-handlers))
       (funcall handler request)))))

(defun acp-fakes-pump (client)
  "Deliver CLIENT's recorded incoming traffic up to the next barrier.

Advances the cursor, routing each incoming message as it is reached, and
stops on an outgoing message the client has not sent yet (see
`acp-fakes-barrier').  Returns the number of messages delivered.

Routing a message can make the code under test send a request or response,
which pumps again; that re-entrant call returns immediately and the
outermost pump carries on from the cursor it left."
  (if (map-elt client :pumping)
      0
    (setf (map-elt client :pumping) t)
    (unwind-protect
        (let ((traffic (acp-fakes--traffic client))
              (delivered 0)
              (blocked nil))
          (while (and (not blocked)
                      (< (map-elt client :cursor) (length traffic)))
            (let* ((index (map-elt client :cursor))
                   (message (aref traffic index)))
              (cond
               ((eq (map-elt message :direction) 'outgoing)
                (if (acp-fakes--claimed-p client index)
                    (setf (map-elt client :cursor) (1+ index))
                  (setq blocked t)))
               (t
                (setf (map-elt client :cursor) (1+ index))
                (setq delivered (1+ delivered))
                (acp-fakes--route client message)))))
          delivered)
      (setf (map-elt client :pumping) nil))))

(defun acp-fakes-barrier (client)
  "Return the recorded outgoing message stalling CLIENT's replay, or nil.

The barrier is traffic the capture sent from the client side that the code
under test has not reproduced.  Sending it resumes the replay; a driver
that will never send it uses `acp-fakes-skip-barrier'."
  (let ((traffic (acp-fakes--traffic client))
        (index (map-elt client :cursor)))
    (when (< index (length traffic))
      (let ((message (aref traffic index)))
        (when (and (eq (map-elt message :direction) 'outgoing)
                   (not (acp-fakes--claimed-p client index)))
          message)))))

(defun acp-fakes-skip-barrier (client)
  "Skip CLIENT's current barrier and resume the replay.

For recorded client traffic the code under test never sends.  Any reply
the recording holds for it is left unclaimed and routes to nothing."
  (when (acp-fakes-barrier client)
    (acp-fakes--claim client (map-elt client :cursor))
    (acp-fakes-pump client)
    t))

(defun acp-fakes-exhausted-p (client)
  "Return non-nil when CLIENT has replayed all recorded traffic."
  (>= (map-elt client :cursor) (length (acp-fakes--traffic client))))

(cl-defun acp-fakes--request-sender (&key client request buffer on-success on-failure)
  "Claim the recorded counterpart of REQUEST and resume CLIENT's replay.

REQUEST is matched to the first unclaimed recorded outgoing request of the
same method, so a driver whose request sequence differs from the capture
by an unrelated request still pairs each turn with its own reply.

Callbacks are registered under the recorded id in acp's own pending-request
shape, so the reply resolves through `acp--route-incoming-message' when the
pump reaches it -- keeping the request in flight, and its notifications in
turn, until then."
  (unless client
    (error ":client is required"))
  (let* ((method (map-elt request :method))
         (recorded (acp-fakes--claim-outgoing
                    client
                    (lambda (message)
                      (and (eq (map-elt message :kind) 'request)
                           (equal (map-nested-elt message '(:object method)) method)))))
         (request-id (map-nested-elt recorded '(:object id))))
    (when request-id
      (map-put! client :pending-requests
                (cons (cons request-id `((:request . ,request)
                                         (:buffer . ,buffer)
                                         (:on-success . ,on-success)
                                         (:on-failure . ,on-failure)))
                      (map-elt client :pending-requests))))
    (acp-fakes-pump client)))

(cl-defun acp-fakes--response-sender (&key client response)
  "Claim the recorded counterpart of RESPONSE and resume CLIENT's replay.

RESPONSE answers an agent-initiated request (a `session/push', a permission
prompt), so it is matched by the id it replies to."
  (unless client
    (error ":client is required"))
  (let ((request-id (map-elt response :request-id)))
    (acp-fakes--claim-outgoing
     client
     (lambda (message)
       (and (eq (map-elt message :kind) 'response)
            (or (null request-id)
                (equal (map-nested-elt message '(:object id)) request-id)))))
    (acp-fakes-pump client)))

(cl-defun acp-fakes--notification-sender (&key client notification)
  "Claim the recorded counterpart of NOTIFICATION and resume CLIENT's replay."
  (unless client
    (error ":client is required"))
  (let ((method (map-elt notification :method)))
    (acp-fakes--claim-outgoing
     client
     (lambda (message)
       (and (eq (map-elt message :kind) 'notification)
            (equal (map-nested-elt message '(:object method)) method)))))
  (acp-fakes-pump client))

(defun acp-fakes--test-fake-client ()
  "Test a fake client."
  (let* ((messages '(((:kind . outgoing)
                      (:object (jsonrpc . "2.0") (method . "initialize") (id . 1)
                               (params (protocolVersion . 1)
                                       (clientCapabilities
                                        (fs (readTextFile . :false)
                                            (writeTextFile . :false))))))
                     ((:kind . incoming)
                      (:object (jsonrpc . "2.0") (id . 1)
                               (result (protocolVersion . 1)
                                       (authMethods
                                        . [((id . "oauth-personal")
                                            (name . "Log in with Google")
                                            (description . :null))
                                           ((id . "gemini-api-key")
                                            (name . "Use Gemini API key")
                                            (description
                                             . "Requires setting the `GEMINI_API_KEY` environment variable"))
                                           ((id . "vertex-ai") (name . "Vertex AI")
                                            (description . :null))])
                                       (agentCapabilities (loadSession . :false)
                                                          (promptCapabilities (image . t)
                                                                              (audio . t)
                                                                              (embeddedContext
                                                                               . t))))))))
         (client (acp-fakes-make-client messages)))

    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (message "RECEIVED NOTIFICATION: %s" notification)))

    (acp-subscribe-to-requests
     :client client
     :on-request (lambda (request)
                   (message "RECEIVED REQUEST: %s" request)))

    (acp-send-request
     :client client
     :request (acp-make-initialize-request
               :protocol-version 1
               :read-text-file-capability nil
               :write-text-file-capability nil)
     :on-success (lambda (result)
                   (message "Initialize succeeded: %s" result))
     :on-failure (lambda (error)
                   (message "Initialize failed: %s" error)))))

(cl-defun acp-fakes-replay (&key client on-outgoing)
  "Replay messages from CLIENT's message queue.
For each outgoing message, call ON-OUTGOING with its :object.
For incoming messages without an id, log them."
  (cl-flet ((acp--log (&rest _) (ignore))
            (acp--log-traffic (&rest _) (ignore)))
    (dolist (msg (map-elt client :message-queue))
      (cond
       ((eq (map-elt msg :direction) 'outgoing)
        (when on-outgoing
          (funcall on-outgoing (map-elt msg :object))))
       ((and (eq (map-elt msg :direction) 'incoming)
             (or (not (map-elt (map-elt msg :object) 'id))
                 (map-elt (map-elt msg :object) 'method)))
        (acp--route-incoming-message
         :message msg
         :client client
         :on-notification
         (lambda (notification)
           (dolist (handler (map-elt client :notification-handlers))
             (funcall handler notification)))
         :on-request
         (lambda (request)
           (dolist (handler (map-elt client :request-handlers))
             (funcall handler request)))))))))

(cl-defun acp-fakes--get-authenticate-request (&key messages)
  "Find the first authentication object in MESSAGES."
  (unless messages
    (error ":messages is required"))
  (seq-find (lambda (item)
              (and (eq (map-elt item :direction) 'outgoing)
                   (equal (map-nested-elt item '(:object method))
                          "authenticate")))
            messages))

(provide 'acp-fakes)

;;; acp-fakes.el ends here
