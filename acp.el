;;; acp.el --- An ACP (Agent Client Protocol) implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/acp.el
;; Version: 0.7.1
;; Package-Requires: ((emacs "28.1"))

(defconst acp-package-version "0.7.1")

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
;; acp.el implements ACP (Agent Client Protocol) as per spec
;; https://agentclientprotocol.com
;;
;; Note: This package is in the very early stage and is likely
;; incomplete or may have some rough edges.
;;
;; Report issues at https://github.com/xenodium/acp.el/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'json)
(eval-when-compile
  (require 'cl-lib))
(require 'acp-traffic)

(defconst acp--jsonrpc-version "2.0")

(defvar acp-logging-enabled nil)

(defvar acp-instance-count 0)

(cl-defun acp-make-client (&key context-buffer command command-params environment-variables
                                request-sender notification-sender request-resolver
                                response-sender)
  "Create an ACP client.

This returns a alist representing the client.

CONTEXT-BUFFER becomes `current-buffer' for all APIs invoking callbacks
unless overridden by them.  For example:

  - `acp-send-request'
  - `acp-subscribe-to-notifications'
  - `acp-subscribe-to-requests'
  - `acp-subscribe-to-errors'

COMMAND is the binary or command-line utility to invoke.
COMMAND-PARAMS is a list of strings for command arguments.
ENVIRONMENT-VARIABLES is a list of strings in the form \"VAR=foo\".

REQUEST-SENDER, NOTIFICATION-SENDER, REQUEST-RESOLVER, and RESPONSE-SENDER are
functions for advanced customization or testing."
  (unless command
    (error ":command is required"))
  (list (cons :context-buffer context-buffer)
        (cons :instance-count (acp--increment-instance-count))
        (cons :process nil)
        (cons :command command)
        (cons :command-params command-params)
        (cons :environment-variables environment-variables)
        (cons :pending-requests ())
        (cons :request-id 0)
        (cons :notification-handlers ())
        (cons :request-handlers ())
        (cons :error-handlers ())
        (cons :request-sender (or request-sender #'acp--request-sender))
        (cons :notification-sender (or notification-sender #'acp--notification-sender))
        (cons :request-resolver (or request-resolver #'acp--request-resolver))
        (cons :response-sender (or response-sender #'acp--response-sender))))

(defun acp--client-started-p (client)
  "Return non-nil if CLIENT process has been started."
  (and (map-elt client :process)
       (process-live-p (map-elt client :process))))

(cl-defun acp--start-client (&key client)
  "Start CLIENT."
  (unless client
    (error ":client is required"))
  (unless (map-elt client :command)
    (error ":command is required"))
  (unless (executable-find (map-elt client :command))
    (error "\"%s\" command line utility not found.  Please install it" (map-elt client :command)))
  (when (acp--client-started-p client)
    (error "Client already started"))
  (let* ((pending-input "")
         (message-queue nil)
         (message-queue-busy nil)
         (process-environment (append (map-elt client :environment-variables)
                                      process-environment))
         (stderr-buffer (get-buffer-create (format "acp-client-stderr(%s)-%s"
                                                   (map-elt client :command)
                                                   (map-elt client :instance-count))))
         (stderr-proc (make-pipe-process
                       :name (format "acp-client-stderr(%s)-%s"
                                     (map-elt client :command)
                                     (map-elt client :instance-count))
                       :buffer stderr-buffer
                       :filter (lambda (_process raw-output)
                                 (acp--log client "STDERR" "%s" (string-trim raw-output))
                                 (when-let ((api-error (acp--parse-stderr-api-error raw-output)))
                                   (acp--log client "API-ERROR" "%s" (string-trim raw-output))
                                   (dolist (handler (map-elt client :error-handlers))
                                     (funcall handler api-error)))))))
    (let ((process (make-process
                    :name (format "acp-client(%s)-%s"
                                  (map-elt client :command)
                                  (map-elt client :instance-count))
                    :command (cons (map-elt client :command)
                                   (map-elt client :command-params))
                    :stderr stderr-proc
                    :connection-type 'pipe
                    :filter (lambda (_proc input)
                              (acp--log client "INCOMING TEXT" "%s" input)
                              (setq pending-input (concat pending-input input))
                              (let ((start 0) pos)
                                (while (setq pos (string-search "\n" pending-input start))
                                  (let ((json (substring pending-input start pos)))
                                    (acp--log client "INCOMING LINE" "%s" json)
                                    (when-let* ((object (condition-case nil
                                                            (acp--parse-json json)
                                                          (error
                                                           (acp--log client "JSON PARSE ERROR" "Invalid JSON: %s" json)
                                                           nil))))
                                      (setq message-queue
                                            (append message-queue
                                                    (list (acp--make-message :json json :object object))))
                                      (unless message-queue-busy
                                        (setq message-queue-busy t)
                                        (run-at-time 0 nil
                                                     (lambda ()
                                                       (while message-queue
                                                         (let ((message (car message-queue)))
                                                           (setq message-queue (cdr message-queue))
                                                           (acp--route-incoming-message
                                                            :message message
                                                            :client client
                                                            :on-notification
                                                            (lambda (notification)
                                                              (dolist (handler (map-elt client :notification-handlers))
                                                                (condition-case-unless-debug err
                                                                    (funcall handler notification)
                                                                  (error
                                                                   (acp--log client "NOTIFICATION HANDLER ERROR"
                                                                             "Failed with error: %S" err)))))
                                                            :on-request
                                                            (lambda (request)
                                                              (dolist (handler (map-elt client :request-handlers))
                                                                (condition-case-unless-debug err
                                                                    (funcall handler request)
                                                                  (error
                                                                   (acp--log client "REQUEST HANDLER ERROR"
                                                                             "Failed with error: %S" err))))))))
                                                       (setq message-queue-busy nil))))))
                                  (setq start (1+ pos)))
                                (setq pending-input (substring pending-input start))))
                    :sentinel (lambda (_process _event)
                                (when (process-live-p stderr-proc)
                                  (delete-process stderr-proc))
                                (when (buffer-live-p stderr-buffer)
                                  (kill-buffer stderr-buffer))))))
      (map-put! client :process process))))

(cl-defun acp-subscribe-to-notifications (&key client on-notification buffer)
  "Subscribe to incoming CLIENT notifications.

ON-NOTIFICATION is of the form: (lambda (notification))

and invoked with BUFFER as current."
  (unless client
    (error ":client is required"))
  (unless on-notification
    (error ":on-notification is required"))
  (let ((handlers (map-elt client :notification-handlers)))
    (push (lambda (notification)
            (with-temp-buffer ;; Fallback to a temp buffer
              (with-current-buffer (or (when (buffer-live-p buffer)
                                         buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-notification notification))))
          handlers)
    (map-put! client :notification-handlers handlers)))

(cl-defun acp-subscribe-to-requests (&key client on-request buffer)
  "Subscribe to incoming CLIENT requests.

ON-REQUEST is of the form: (lambda (request))

and invoked with BUFFER as current."
  (unless client
    (error ":client is required"))
  (unless on-request
    (error ":on-request is required"))
  (let ((handlers (map-elt client :request-handlers)))
    (push (lambda (request)
            (with-temp-buffer ;; Fallback to a temp buffer
              (with-current-buffer (or (when (buffer-live-p buffer)
                                         buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-request request))))
          handlers)
    (map-put! client :request-handlers handlers)))

(cl-defun acp-subscribe-to-errors (&key client on-error buffer)
  "Subscribe to agent errors using CLIENT.

ON-ERROR is of the form: (lambda (error))

and invoked with BUFFER as current.

Note: These are agent process errors.
      For request errors refer to corresponding API's on-error."
  (unless client
    (error ":client is required"))
  (unless on-error
    (error ":on-error is required"))
  (let ((handlers (map-elt client :error-handlers)))
    (push (lambda (error)
            (with-temp-buffer ;; Fallback to a temp buffer
              (with-current-buffer (or (when (buffer-live-p buffer)
                                         buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-error error))))
          handlers)
    (map-put! client :error-handlers handlers)))

(cl-defun acp-shutdown (&key client)
  "Shutdown ACP CLIENT and release resources."
  (unless client
    (error ":client is required"))
  (when (process-live-p (map-elt client :process))
    (delete-process (map-elt client :process)))
  (kill-buffer (acp-logs-buffer :client client))
  (kill-buffer (acp-traffic-buffer :client client)))

(cl-defun acp-send-request (&key client request buffer on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).

When non-nil SYNC, send request synchronously.
When BUFFER is provided, callbacks executed within buffer context."
  (unless client
    (error ":client is required"))
  (unless request
    (error ":request is required"))
  (unless (acp--client-started-p client)
    (acp--start-client :client client))
  (funcall (map-elt client :request-sender)
           :client client
           :request request
           :buffer buffer
           :on-success on-success
           :on-failure on-failure
           :sync sync))

(cl-defun acp-send-notification (&key client notification sync)
  "Send NOTIFICATION from CLIENT.

When non-nil SYNC, send notification synchronously."
  (unless client
    (error ":client is required"))
  (unless notification
    (error ":notification is required"))
  (unless (acp--client-started-p client)
    (acp--start-client :client client))
  (funcall (map-elt client :notification-sender)
           :client client
           :notification notification
           :sync sync))

(cl-defun acp--request-sender (&key client request buffer on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).
BUFFER: When non-nil, override CLIENT `:buffer-context' (see `acp-make-client').
SYNC: When non-nil, send request synchronously."
  (unless client
    (error ":client is required"))
  (unless request
    (error ":request is required"))
  (unless (acp--client-started-p client)
    (acp--start-client :client client))
  (let* ((method (map-elt request :method))
         (params (map-elt request :params))
         (proc (map-elt client :process))
         (request-id (1+ (map-elt client :request-id)))
         (request `((jsonrpc . ,acp--jsonrpc-version)
                    (method . ,method)
                    (id . ,request-id)
                    ,@(when params `((params . ,params)))))
         (result nil)
         (done nil))
    (map-put! client :request-id request-id)
    (map-put! client :pending-requests
              (cons (cons request-id `((:request . ,request)
                                       (:buffer . ,buffer)
                                       (:on-success . ,on-success)
                                       (:on-failure . ,on-failure)))
                    (map-elt client :pending-requests)))
    (when sync
      (map-put! (map-nested-elt client `(:pending-requests ,request-id)) :on-success
                (lambda (data)
                  (setq result data
                        done t)))
      (map-put! (map-nested-elt client `(:pending-requests ,request-id)) :on-failure
                (lambda (data)
                  (setq result data
                        done 'error))))
    (acp--log client "OUTGOING OBJECT" "%s" request)
    (let ((json (acp--serialize-json request)))
      (acp--log client "OUTGOING TEXT" "%s" json)
      (acp--log-traffic client 'outgoing 'request (acp--make-message :object request :json json))
      (process-send-string proc json))
    (when sync
      (while (not done)
        (accept-process-output proc 0.01))
      (if (eq done 'error)
          (error "ACP request failed: %s" result)
        result))))

(cl-defun acp-send-response (&key client response)
  "Send a request RESPONSE from CLIENT."
  (unless client
    (error ":client is required"))
  (unless response
    (error ":response is required"))
  (funcall (map-elt client :response-sender)
           :client client
           :response response))

(cl-defun acp--response-sender (&key client response)
  "Send a request RESPONSE from CLIENT."
  (unless client
    (error ":client is required"))
  (unless response
    (error ":response is required"))
  (let* ((request-id (map-elt response :request-id))
         (result-data (map-elt response :result)))
    (map-put! client :request-id (or request-id
                                     (1+ (map-elt client :request-id))))
    (let* ((proc (map-elt client :process))
           (response `((jsonrpc . ,acp--jsonrpc-version)
                       (id . ,request-id)
                       (result . ,result-data))))
      (let ((json (acp--serialize-json response)))
        (acp--log-traffic client 'outgoing 'response (acp--make-message :object response :json json))
        (process-send-string proc json)))))

(cl-defun acp--notification-sender (&key client notification sync)
  "Send NOTIFICATION from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).

When non-nil SYNC, send notification synchronously."
  (unless client
    (error ":client is required"))
  (unless notification
    (error ":notification is required"))
  (unless (acp--client-started-p client)
    (acp--start-client :client client))
  (let* ((method (map-elt notification :method))
         (params (map-elt notification :params))
         (proc (map-elt client :process))
         (notification `((jsonrpc . ,acp--jsonrpc-version)
                         (method . ,method)
                         ,@(when params `((params . ,params)))))
         (result nil)
         (done nil))
    (acp--log client "OUTGOING OBJECT" "%s" notification)
    (let ((json (acp--serialize-json notification)))
      (acp--log client "OUTGOING TEXT" "%s" json)
      (acp--log-traffic client 'outgoing 'notification (acp--make-message :object notification :json json))
      (process-send-string proc json))
    (when sync
      (while (not done)
        (accept-process-output proc 0.01))
      (if (eq done 'error)
          (error "ACP notification failed: %s" result)
        result))))

(cl-defun acp-make-initialize-request (&key protocol-version
                                            read-text-file-capability
                                            write-text-file-capability)
  "Instantiate an \"initialize\" request.

PROTOCOL-VERSION is the version of the ACP protocol to use.
READ-TEXT-FILE-CAPABILITY is a boolean indicating if the client
can read text files.
WRITE-TEXT-FILE-CAPABILITY is a boolean indicating if the client
can write text files.

See https://agentclientprotocol.com/protocol/schema#initializerequest
and https://agentclientprotocol.com/protocol/schema#initializeresponse."
  (unless protocol-version
    (error ":protocol-version is required"))
  `((:method . "initialize")
    (:params . ((protocolVersion . ,protocol-version)
                (clientCapabilities . ((fs . ((readTextFile . ,(if read-text-file-capability
                                                                   t
                                                                 :false))
                                              (writeTextFile . ,(if write-text-file-capability
                                                                    t
                                                                  :false))))))))))

(cl-defun acp-make-authenticate-request (&key method-id)
  "Instantiate an \"authenticate\" request.

METHOD-ID is the authentication method to use.

See https://agentclientprotocol.com/protocol/schema#authenticaterequest."
  (unless method-id
    (error ":method-id is required"))
  `((:method . "authenticate")
    (:params . ((methodId . ,method-id)))))

(cl-defun acp-make-session-new-request (&key cwd mcp-servers)
  "Instantiate a \"session/new\" request.

CWD is the current working directory for the session.
MCP-SERVERS is a list of MCP servers to use.

See https://agentclientprotocol.com/protocol/schema#newsessionrequest
and https://agentclientprotocol.com/protocol/schema#newsessionresponse."
  (unless cwd
    (error ":cwd is required"))
  `((:method . "session/new")
    (:params . ((cwd . ,(expand-file-name cwd))
                (mcpServers . ,(or mcp-servers []))))))

(cl-defun acp-make-session-prompt-request (&key session-id prompt)
  "Instantiate a \"session/prompt\" request.

SESSION-ID is the ID of the session to send the prompt to.
PROMPT is the prompt string.

See https://agentclientprotocol.com/protocol/schema#promptrequest
and https://agentclientprotocol.com/protocol/schema#promptresponse."
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . "session/prompt")
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))))))

(cl-defun acp-make-session-set-mode-request (&key session-id mode-id)
  "Instantiate a \"session/set_mode\" request.

SESSION-ID is the ID of the session to change the mode for.
MODE-ID is the mode to set (e.g., \"default\", \"plan\", \"acceptEdits\",
\"bypassPermissions\").

See https://agentclientprotocol.com/protocol/session-modes"
  (unless session-id
    (error ":session-id is required"))
  (unless mode-id
    (error ":mode-id is required"))
  `((:method . "session/set_mode")
    (:params . ((sessionId . ,session-id)
                (modeId . ,mode-id)))))

(cl-defun acp-make-session-request-permission-response (&key request-id option-id cancelled)
  "Instantiate a \"session/request_permission\" response.

REQUEST-ID is the ID of the request this is a response to.
OPTION-ID is the ID of the option selected by the user.
CANCELLED is non-nil if the response represents a cancelled request.

See https://agentclientprotocol.com/protocol/schema#requestpermissionresponse."
  (unless request-id
    (error ":request-id is required"))
  (when (and option-id cancelled)
    (error "Choose :option-id or :cancelled Not both"))
  (unless (or option-id cancelled)
    (error "Must specify either :option-id or :cancelled"))
  `((:request-id . ,request-id)
    (:result . ((outcome . ,(if cancelled
                                 '((outcome . "cancelled"))
                               `((outcome . "selected")
                                 (optionId . ,option-id))))))))

(cl-defun acp-make-fs-read-text-file-response (&key request-id content error)
  "Instantiate a \"fs/read_text_file\" response.

REQUEST-ID is the ID of the request this is a response to.
Provide either CONTENT (the file content as a string) or ERROR,
but not both.

See https://agentclientprotocol.com/protocol/schema#readtextfileresponse."
  (unless request-id
    (error ":request-id is required"))
  (cond
   ((and content error)
    (error "Either :content or :error but not both"))
   (error
    `((:request-id . ,request-id)
      (:error . ,error)))
   (content
    `((:request-id . ,request-id)
      (:result . ((content . ,content)))))
   (t
    (error "Either :content or :error is required"))))

(cl-defun acp-make-fs-write-text-file-response (&key request-id error)
  "Instantiate a \"fs/write_text_file\" response.

REQUEST-ID is the ID of the request this is a response to.
ERROR is an optional error object if the write operation failed.

See https://agentclientprotocol.com/protocol/schema#writetextfileresponse."
  (unless request-id
    (error ":request-id is required"))
  (if error
      `((:request-id . ,request-id)
        (:error . ,error))
    `((:request-id . ,request-id)
      (:result . nil))))

(cl-defun acp-make-error (&key code message data)
  "Create a JSON-RPC error object.

CODE is the error code.
MESSAGE is a string providing a short description of the error.
DATA is an optional value that contains additional information.

See https://www.jsonrpc.org/specification#error_object."
  (unless code
    (error ":code is required"))
  (unless message
    (error ":message is required"))
  (let ((error `((code . ,code)
                 (message . ,message))))
    (when data
      (nconc error `((data . ,data))))
    error))

(cl-defun acp-make-session-cancel-notification (&key session-id reason)
  "Instantiate a \"session/cancel\" request.

SESSION-ID is the ID of the session to cancel.
REASON is an optional string explaining the reason for cancellation.

See https://agentclientprotocol.com/protocol/schema#cancelnotification."
  (unless session-id
    (error ":session-id is required"))
  `((:method . "session/cancel")
    (:params . ((sessionId . ,session-id)
                ,@(when reason `((reason . ,reason)))))))

(cl-defun acp--request-resolver (&key client id)
  "Resolve CLIENT request with ID to a handler."
  (map-nested-elt client `(:pending-requests ,id)))

(cl-defun acp--make-message (&key json object)
  "Create message with JSON and OBJECT."
  (list (cons :object object)
        (cons :json json)))

(cl-defun acp--route-incoming-message (&key client message on-notification on-request)
  "Parse CLIENT's incoming MESSAGE with json/object and route accordingly.

ON-NOTIFICATION is of the form (lambda (notification))
ON-REQUEST is of the form (lambda (request))."
  (unless message
    (error ":object is required"))
  (unless on-notification
    (error ":on-notification is required"))
  (unless on-request
    (error ":on-request is required"))
  (let-alist (map-elt message :object)
    (or
     ;; Method request result (success)
     (when-let ((incoming-response (and .id
                                        ;; Must check against key and not value because
                                        ;; nil result is valid also.
                                        (map-contains-key (map-elt message :object) 'result)
                                        (funcall (map-elt client :request-resolver)
                                                 :client client :id .id))))
       (acp--log client nil "↳ Routing as response (result)")
       (acp--log-traffic client 'incoming 'response message)
       (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt incoming-response :on-success)
           (with-temp-buffer ;; Fallback to a temp buffer
             (with-current-buffer (or (map-elt incoming-response :buffer)
                                      (map-elt client :context-buffer)
                                      (current-buffer))
               (funcall (map-elt incoming-response :on-success) .result)))
         ;; TODO: Consolidate serialization.
         (acp--log client nil "Unhandled result:\n\n%s" message))
       t)

     ;; Method request result (failure)
     (when-let ((incoming-response (and .error .id
                                        (funcall (map-elt client :request-resolver)
                                                 :client client :id .id))))
       (acp--log client nil "↳ Routing as response (error)")
       (acp--log-traffic client 'incoming 'response message)
       (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt incoming-response :on-failure)
           (with-temp-buffer ;; Fallback to a temp buffer
             (with-current-buffer (or (map-elt incoming-response :buffer)
                                      (map-elt client :context-buffer)
                                      (current-buffer))
               (let ((callback (map-elt incoming-response :on-failure)))
                 (if (>= (cdr (func-arity callback)) 2)
                     (funcall callback .error message)
                   (funcall callback .error)))))
         (acp--log client nil "Unhandled error:\n\n%s" message))
       t)

     ;; Incoming method request
     (when (and .method .id)
       (acp--log client nil "↳ Routing as incoming request")
       (acp--log-traffic client 'incoming 'request message)
       (when on-request
         (funcall on-request (map-elt message :object)))
       t)

     ;; Incoming notification
     (when (not .id)
       (acp--log client nil "↳ Routing as notification")
       (acp--log-traffic client 'incoming 'notification message)
       (when on-notification
         (funcall on-notification (map-elt message :object)))
       t)

     ;; Unrecognized
     (when t
       (acp--log client nil "↳ Routing undefined (could not recognize)\n\n%s" (map-elt message :object))
       (acp--log-traffic client 'incoming 'unknown message)))))

(cl-defun acp--parse-stderr-api-error (raw-output)
  "Parse RAW-OUTPUT, typically from stderr.

Returns non-nil if error was parseable."
  (when (string-match "Attempt \\([0-9]+\\) failed with status \\([0-9]+\\)\\. Retrying.*ApiError: \\({.*}\\)" raw-output)
    (let ((error-json (match-string 3 raw-output)))
      (condition-case nil
          (let-alist (acp--parse-json error-json)
            ;; Parse the inner JSON from the message field and return just the error part
            (condition-case nil
                (map-elt (acp--parse-json .error.message) 'error)
              (error nil)))
        (error nil)))))

(defun acp--log (client label format-string &rest args)
  "Log CLIENT message using LABEL, FORMAT-STRING, and ARGS."
  (unless format-string
    (error ":format-string is required"))
  (when acp-logging-enabled
    (let ((log-buffer (acp-logs-buffer :client client)))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (if label
            (insert label " >\n\n" (apply #'format format-string args) "\n\n")
          (insert (apply #'format format-string args) "\n\n"))))))

(defun acp--json-pretty-print (json)
  "Return a pretty-printed JSON string."
  (if acp-logging-enabled
      (with-temp-buffer
        (insert json)
        (json-pretty-print (point-min) (point-max))
        (buffer-string))
    json))

(defun acp--log-traffic (client direction kind message)
  "Log CLIENT traffic MESSAGE to \"*acp traffic*\" buffer.
KIND may be `request', `response', or `notification'.
DIRECTION is either `incoming' or `outgoing', OBJECT is the parsed object."
  (when acp-logging-enabled
    (acp-traffic-log-traffic
     :buffer (acp-traffic-buffer :client client)
     :direction direction :kind kind :message message)))

(defun acp--show-json-object (object)
  "Display OBJECT in a pretty-printed buffer."
  (let ((json-buffer (get-buffer-create "*acp object*")))
    (with-current-buffer json-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (json-encode object))
      (json-pretty-print-buffer)
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer json-buffer)))

(cl-defun acp-reset-logs (&key client)
  "Reset CLIENT log buffers."
  (with-current-buffer (acp-logs-buffer :client client)
    (erase-buffer))
  (with-current-buffer (acp-traffic-buffer :client client)
    (erase-buffer)))

(cl-defun acp-logs-buffer (&key client)
  "Get CLIENT logs buffer."
  (get-buffer-create (format "*acp-(%s)-%s log*"
                             (map-elt client :command)
                             (map-elt client :instance-count))))

(cl-defun acp-traffic-buffer (&key client)
  "Get CLIENT traffic buffer."
  (acp-traffic-get-buffer :named (format "*acp-(%s)-%s traffic*"
                                         (map-elt client :command)
                                         (map-elt client :instance-count))))

(defun acp--increment-instance-count ()
  "Increment variable `acp-instance-count'."
  (if (= acp-instance-count most-positive-fixnum)
      (setq acp-instance-count 0)
    (setq acp-instance-count (1+ acp-instance-count))))

(defun acp--parse-json (json)
  "Parse JSON using a consistent configuration."
  (json-parse-string json :object-type 'alist :null-object nil))

(defun acp--serialize-json (object)
  "Serialize OBJECT to JSON using a consistent configuration."
  (concat (json-serialize object) "\n"))

(provide 'acp)

;;; acp.el ends here
