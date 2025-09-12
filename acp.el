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

(cl-defun acp--make-client (&key process)
  "Make an internal client using PROCESS."
  (unless process
    (error ":process is required"))
  (list (cons :process process)
        (cons :pending-requests ())
        (cons :request-id 0)
        (cons :notification-handlers ())
        (cons :request-handlers ())
        (cons :error-handlers ())))

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

(cl-defun acp-make-client (&key command command-params environment-variables)
  "Create generic ACP client with process.

For example:

  COMMAND: gemini
  COMMAND-PARAMS: --experimental-acp
  ENVIRONMENT-VARIABLES: (\"GEMINI_API_KEY=123\")"
  (unless command
    (error ":command is required"))
  (unless (executable-find command)
    (error "%s not found.  Please install" command))
  (let* ((pending-input "")
         (client nil)
         (process-environment (append environment-variables process-environment))
         (stderr-proc (make-pipe-process
                       :name (format "acp-client-stderr(%s)" command)
                       :filter (lambda (_process raw-output)
                                 (acp--log "STDERR" "%s" (string-trim raw-output))
                                 (when-let ((api-error (acp--parse-stderr-api-error raw-output)))
                                   (acp--log "API-ERROR" "%s" (string-trim raw-output))
                                   (dolist (handler (alist-get :error-handlers client))
                                     (funcall handler api-error))))
                       :sentinel (lambda (process _event)
                                   (when (process-buffer process)
                                     (kill-buffer (process-buffer process)))))))
    (let ((process (make-process
                    :name (format "acp-client(%s)" command)
                    :command (cons command command-params)
                    :stderr stderr-proc
                    :filter (lambda (_proc input)
                              (acp--log "INCOMING TEXT" "%s" input)
                              (setq pending-input (concat pending-input input))
                              (while (string-match "\\(.*\\)\n" pending-input)
                                (acp--log "INCOMING LINE" "%s" (match-string 1 pending-input))
                                (let* ((json (match-string 1 pending-input)))
                                  (setq pending-input (substring pending-input (match-end 0)))
                                  (acp--route-incoming-event :json json
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
      (setq client (acp--make-client :process process))
      client)))

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
    (delete-process (map-elt client :process))))

(cl-defun acp-send-request (&key client request on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is of the form (lambda (response)).
ON-FAILURE is of the form (lambda (error)).

When non-nil SYNC, send request synchronously."
  (unless client
    (error ":client is required"))
  (unless request
    (error ":request is required"))
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
    (let ((json (concat (json-serialize request) "\n")))
      (acp--json-log "OUTGOING REQUEST"
                     (with-temp-buffer
                       (insert json)
                       (json-pretty-print (point-min) (point-max))
                       (buffer-string)))
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
  (let* ((request-id (map-elt response :request-id))
         (result-data (map-elt response :result)))
    (map-put! client :request-id (or request-id
                                     (1+ (map-elt client :request-id))))
    (let* ((proc (map-elt client :process))
           (response `((jsonrpc . ,acp--jsonrpc-version)
                       (id . ,request-id)
                       (result . ,result-data))))
      (let ((data (concat (json-serialize response) "\n")))
        (acp--json-log "OUTGOING RESULT" data)
        (process-send-string proc data)))))

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
  "Instantiate an \"initialize\" request.

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

(cl-defun acp--route-incoming-event (&key client json on-notification on-request)
  "Parse CLIENT's incoming JSON event and route as notification or request.

ON-NOTIFICATION is of the form (lambda (notification))
ON-REQUEST is of the form (lambda (request))."
  (unless client
    (error ":client is required"))
  (unless json
    (error ":json is required"))
  (unless on-notification
    (error ":on-notification is required"))
  (unless on-request
    (error ":on-request is required"))
  (when-let ((logging t))
    (acp--log "INCOMING JSON" "%s"
              (with-temp-buffer
                (insert json)
                (json-pretty-print (point-min) (point-max))
                (buffer-string)))
    (let ((object (json-parse-string json :object-type 'alist)))
      (let-alist object
        (acp--json-log (format "INCOMING (%s)" .method)
                       (with-temp-buffer
                         (insert json)
                         (json-pretty-print (point-min) (point-max))
                         (buffer-string)))
        (cond
         ;; Method request result (success)
         ((and .result .id
               (map-contains-key (map-elt client :pending-requests) .id)
               (equal .method
                      (map-nested-elt client `(:pending-requests ,.id :request :method))))
          (acp--log nil "↳ Routing as response (result)")
          (let ((on-success (map-nested-elt (map-elt client :pending-requests)
                                            (list .id :on-success))))
            (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
            (if on-success
                (funcall on-success .result)
              (acp--log nil "Unhandled result:\n\n%s" json))))
         ;; Method request result (failure)
         ((and .error .id
               (map-contains-key (map-elt client :pending-requests) .id))
          (acp--log nil "↳ Routing as response (result)")
          (let ((on-failure (map-nested-elt (map-elt client :pending-requests)
                                            (list .id :on-failure))))
            (map-put! client :pending-requests (map-delete (map-elt client :pending-requests) .id))
            (if on-failure
                (if (>= (cdr (func-arity on-failure)) 2)
                    (funcall on-failure .error json)
                  (funcall on-failure .error))
              (acp--log nil "Unhandled error:\n\n%s" json))))
         ;; Incoming method request
         ((and .method .id)
          (acp--log nil "↳ Routing as incoming request")
          (when on-request
            (funcall on-request object)))
         ((not .id)
          (acp--log nil "↳ Routing as notification")
          (when on-notification
            (funcall on-notification object)))
         (t
          (acp--log nil "↳ Routing undefined (could not recognize)")))))))

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

(defun acp--log (label format-string &rest args)
  "Log message using LABEL, FORMAT-STRING, and ARGS."
  (unless format-string
    (error ":format-string is required"))
  (let ((log-buffer (get-buffer-create "*acp log*")))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (if label
          (insert label " >\n\n" (apply #'format format-string args) "\n\n")
        (insert (apply #'format format-string args))))))

(defun acp--json-log (label json)
  "Log to json buffer using LABEL and JSON."
  (unless label
    (error ":label is required"))
  (unless json
    (error ":json is required"))
  (let ((log-buffer (get-buffer-create "*acp json log*")))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (concat "\n\n" label " >\n\n" json)))))

(provide 'acp)

;;; acp.el ends here
