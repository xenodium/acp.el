;;; acp.el --- An ACP (Agent Client Protocol) implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/acp.el
;; Version: 0.1.1

(defconst acp-package-version "0.1.1")

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
(require 'cl-lib)

(defconst acp--jsonrpc-version "2.0")

(defvar acp-logging-enabled nil)

(defvar acp-instance-count 0)

(cl-defun acp-make-client (&key command command-params environment-variables
                                request-sender request-resolver response-sender)
  "Create an ACP client.

COMMAND: Binary or command line utility to invoke.
COMMAND-PARAMS: Command params.
ENVIRONMENT-VARIABLES: In the form `(\"VAR=foo\").

The following are only neede if you'd like to mock clients.

REQUEST-SENDER: Sends requests.
REQUEST-RESOLVER: Resolves incoming requests to handler.
RESPONSE-SENDER: Sends responses."
  (unless command
    (error ":command is required"))
  (unless (executable-find command)
    (error "%s not found.  Please install" command))
  (list (cons :instance-count (acp--increment-instance-count))
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
        (cons :request-resolver (or request-resolver #'acp--request-resolver))
        (cons :response-sender (or response-sender #'acp--response-sender))))

(cl-defun acp-make-gemini-client (&key api-key)
  "Create a Gemini ACP client with API-KEY.

https://github.com/google-gemini/gemini-cli"
  (unless api-key
    (error ":api-key is required"))
  (acp-make-client :command "gemini"
                   :command-params '("--experimental-acp")
                   :environment-variables (when api-key
                                            (list (format "GEMINI_API_KEY=%s" api-key)))))

(cl-defun acp-make-claude-client (&key api-key)
  "Create Claude Code ACP client with API-KEY.

https://www.anthropic.com/claude-code"
  (unless api-key
    (error ":api-key is required"))
  (acp-make-client :command "claude-code-acp"
                   :environment-variables (when api-key
                                            (list (format "ANTHROPIC_API_KEY=%s" api-key)))))

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
    (error "%s not found.  Please install it" (map-elt client :command)))
  (when (acp--client-started-p client)
    (error "Client already started"))
  (let* ((pending-input "")
         (process-environment (append (map-elt client :environment-variables)
                                      process-environment))
         (stderr-proc (make-pipe-process
                       :name (format "acp-client-stderr(%s)-%s"
                                     (map-elt client :command)
                                     (map-elt client :instance-count))
                       :buffer nil
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
                              (while (string-match "\\(.*\\)\n" pending-input)
                                (acp--log client "INCOMING LINE" "%s" (match-string 1 pending-input))
                                (when-let* ((json (match-string 1 pending-input))
                                            (object (condition-case nil
                                                        (json-parse-string json :object-type 'alist)
                                                      (error
                                                       (acp--log client "JSON PARSE ERROR" "Invalid JSON: %s" json)
                                                       nil))))
                                  (setq pending-input (substring pending-input (match-end 0)))
                                  (acp--route-incoming-message
                                   :message (acp--make-message :json json :object object)
                                   :client client
                                   :on-notification
                                   (lambda (notification)
                                     (dolist (handler (map-elt client :notification-handlers))
                                       (funcall handler notification)))
                                   :on-request
                                   (lambda (request)
                                     (dolist (handler (map-elt client :request-handlers))
                                       (funcall handler request)))))))
                    :sentinel (lambda (_process _event)
                                (when (process-live-p stderr-proc)
                                  (delete-process stderr-proc))))))
      (map-put! client :process process))))

(cl-defun acp-subscribe-to-notifications (&key client on-notification)
  "Subscribe to incoming CLIENT notifications.

ON-NOTIFICATION is of the form: (lambda (notification))"
  (unless client
    (error ":client is required"))
  (unless on-notification
    (error ":on-notification is required"))
  (let ((handlers (map-elt client :notification-handlers)))
    (push on-notification handlers)
    (map-put! client :notification-handlers handlers)))

(cl-defun acp-subscribe-to-requests (&key client on-request)
  "Subscribe to incoming CLIENT requests.

ON-REQUEST is of the form: (lambda (request))"
  (unless client
    (error ":client is required"))
  (unless on-request
    (error ":on-request is required"))
  (let ((handlers (map-elt client :request-handlers)))
    (push on-request handlers)
    (map-put! client :request-handlers handlers)))

(cl-defun acp-subscribe-to-errors (&key client on-error)
  "Subscribe to agent errors using CLIENT.

ON-ERROR is of the form: (lambda (error))

Note: These are agent process errors.
      For request errors refer to corresponding API's on-error."
  (unless client
    (error ":client is required"))
  (unless on-error
    (error ":on-error is required"))
  (let ((handlers (map-elt client :error-handlers)))
    (push on-error handlers)
    (map-put! client :error-handlers handlers)))

(cl-defun acp-shutdown (&key client)
  "Shutdown ACP CLIENT and release resources."
  (unless client
    (error ":client is required"))
  (when (process-live-p (map-elt client :process))
    (delete-process (map-elt client :process)))
  (kill-buffer (acp-logs-buffer :client client))
  (kill-buffer (acp-traffic-buffer :client client)))

(cl-defun acp-send-request (&key client request on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).

When non-nil SYNC, send request synchronously."
  (unless client
    (error ":client is required"))
  (unless request
    (error ":request is required"))
  (unless (acp--client-started-p client)
    (acp--start-client :client client))
  (funcall (map-elt client :request-sender)
           :client client
           :request request
           :on-success on-success
           :on-failure on-failure
           :sync sync))

(cl-defun acp--request-sender (&key client request on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).

When non-nil SYNC, send request synchronously."
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
    (let ((json (concat (json-serialize request) "\n")))
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
      (let ((json (concat (json-serialize response) "\n")))
        (acp--log-traffic client 'outgoing 'response (acp--make-message :object response :json json))
        (process-send-string proc json)))))

(cl-defun acp-make-initialize-request (&key protocol-version
                                            read-text-file-capability
                                            write-text-file-capability)
  "Instantiate an \"initialize\" request.

See request's PROTOCOL-VERSION, READ-TEXT-FILE-CAPABILITY, and
WRITE-TEXT-FILE-CAPABILITY usage at:
https://agentclientprotocol.com/protocol/schema#initializerequest for


See response https://agentclientprotocol.com/protocol/schema#initializeresponse."
  (unless protocol-version
    (error ":protocol-version is required"))
  `((:method . "initialize")
    (:params . ((protocolVersion . ,protocol-version)
                (clientCapabilities . ((fs . ((readTextFile . ,(if read-text-file-capability
                                                                   :true
                                                                 :false))
                                              (writeTextFile . ,(if write-text-file-capability
                                                                    :true
                                                                  :false))))))))))

(cl-defun acp-make-authenticate-request (&key method-id)
  "Instantiate an \"authenticate\" request.

See request's METHOD-ID usage at:
https://agentclientprotocol.com/protocol/schema#authenticaterequest."
  (unless method-id
    (error ":method-id is required"))
  `((:method . "authenticate")
    (:params . ((methodId . ,method-id)))))

(cl-defun acp-make-session-new-request (&key cwd mcp-servers)
  "Instantiate a \"session/new\" request.

See request's CWD and MCP-SERVERS usage at
https://agentclientprotocol.com/protocol/schema#newsessionrequest

See response https://agentclientprotocol.com/protocol/schema#newsessionresponse."
  (unless cwd
    (error ":cwd is required"))
  `((:method . "session/new")
    (:params . ((cwd . ,cwd)
                (mcpServers . ,(or mcp-servers []))))))

(cl-defun acp-make-session-prompt-request (&key session-id prompt)
  "Instantiate an \"session/prompt\" request.

See request https://agentclientprotocol.com/protocol/schema#promptrequest for
SESSION-ID and PROMPT usage.

See response https://agentclientprotocol.com/protocol/schema#promptresponse."
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . "session/prompt")
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))))))

(cl-defun acp-make-session-request-permission-response (&key request-id option-id)
  "Instantiate a \"session/request_permission\" response.

See response's REQUEST-ID and OPTION-ID usage at
https://agentclientprotocol.com/protocol/schema#requestpermissionresponse."
  (unless request-id
    (error ":request-id is required"))
  (unless option-id
    (error ":option-id is required"))
  `((:request-id . ,request-id)
    (:result . ((outcome . ((outcome . "selected")
                            (optionId . ,option-id)))))))

(cl-defun acp-make-session-cancel-request (&key session-id reason)
  "Instantiate a \"session/cancel\" request.

SESSION-ID is required and should be the ID of the session to cancel.

REASON is an optional string explaining the cancellation reason.

See https://agentclientprotocol.com/protocol/schema#sessioncancelrequest for details."
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
  (list (cons :json json)
        (cons :object object)))

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
     (when-let ((incoming-response (and .result .id
                                        (funcall (map-elt client :request-resolver)
                                                 :client client :id .id))))
       (acp--log client nil "↳ Routing as response (result)")
       (acp--log-traffic client 'incoming 'response message)
       (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt incoming-response :on-success)
           (funcall (map-elt incoming-response :on-success) .result)
         ;; TODO: Consolidate serialization.
         (acp--log client nil "Unhandled result:\n\n%s" (or (map-elt message :object)
                                                            (map-elt message :json))))
       t)

     ;; Method request result (failure)
     (when-let ((incoming-response (and .error .id
                                        (funcall (map-elt client :request-resolver)
                                                 :client client :id .id))))
       (acp--log client nil "↳ Routing as response (error)")
       (acp--log-traffic client 'incoming 'response message)
       (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt incoming-response :on-failure)
           (if (>= (cdr (func-arity (map-elt incoming-response :on-failure))) 2)
               (funcall (map-elt incoming-response :on-failure)
                        .error (or (map-elt message :object)
                                   (map-elt message :json)))
             (funcall (map-elt incoming-response :on-failure) .error))
         (acp--log client nil "Unhandled error:\n\n%s" (or (map-elt message :object)
                                                           (map-elt message :json))))
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
          (let-alist (json-parse-string error-json :object-type 'alist)
            ;; Parse the inner JSON from the message field and return just the error part
            (condition-case nil
                (map-elt (json-parse-string .error.message :object-type 'alist) 'error)
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
          (insert (apply #'format format-string args)))))))

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
  (let ((inhibit-read-only t)
        (traffic-buffer (acp-traffic-buffer :client client)))
    (with-current-buffer traffic-buffer
      (goto-char (point-max))
      (let* ((object (map-elt message :object))
             (timestamp (format-time-string "%H:%M:%S.%3N"))
             (method (map-elt object 'method))
             (has-result (map-elt object 'result))
             (has-error (map-elt object 'error))
             (method-info (or method
                              (when has-result
                                "result")
                              (when has-error
                                "error")
                              "unknown"))
             (line-text (format "%s %s %-12s %s\n"
                                timestamp
                                (propertize (if (eq direction 'incoming) "←" "→")
                                            'face (if (eq direction 'incoming)
                                                      'success
                                                    'error))
                                kind
                                (propertize method-info 'face font-lock-function-name-face)))
             (full-object `((:direction . ,direction)
                            (:kind . ,kind)
                            (:object . ,object)))
             (action-keymap (let ((map (make-sparse-keymap)))
                              (define-key map [mouse-1]
                                          (lambda ()
                                            (interactive)
                                            (acp--show-json-object object)))
                              (define-key map (kbd "RET")
                                          (lambda ()
                                            (interactive)
                                            (acp--show-json-object object)))
                              (define-key map [remap self-insert-command] 'ignore)
                              map)))
        (add-text-properties 0 (length line-text)
                             `(keymap ,action-keymap
                                      json-object ,object)
                             line-text)
        (add-text-properties 0 (length line-text)
                             `(acp-object ,full-object)
                             line-text)
        (insert line-text)))
    ;; Keep buffer size manageable (last 1000 lines)
    (when (> (count-lines (point-min) (point-max)) 1000)
      (goto-char (point-min))
      (forward-line 100)
      (delete-region (point-min) (point)))))

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
  (get-buffer-create (format "*acp-(%s)-%s traffic*"
                             (map-elt client :command)
                             (map-elt client :instance-count))))

(defun acp--increment-instance-count ()
  "Increment variable `acp-instance-count'."
  (if (= acp-instance-count most-positive-fixnum)
      (setq acp-instance-count 0)
    (setq acp-instance-count (1+ acp-instance-count))))

(provide 'acp)

;;; acp.el ends here
