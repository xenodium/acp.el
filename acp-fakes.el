;;; acp-fakes.el --- A fake ACP client -*- lexical-binding: t; -*-

(require 'acp)
(require 'map)
(eval-when-compile (require 'cl))

(defun acp-fakes-make-client (messages)
  "Create a fake ACP client that responds using traffic MESSAGES.
Each message is an alist with :kind :object and :json values."
  (let ((client (acp-make-client
                 :command "cat"
                 :command-params nil
                 :environment-variables nil
                 :request-sender (cl-function (lambda (&key client request on-success on-failure _sync)
                                                (acp-fakes--request-sender
                                                 :client client
                                                 :request request
                                                 :on-success on-success
                                                 :on-failure on-failure)))
                 :response-sender
                 (cl-function (lambda (&key _client response)
                                (acp-fakes--response-sender :response response)))
                 :request-resolver
                 (cl-function (lambda (&key client id)
                                (acp-fakes--request-resolver :client client :id id))))))
    (setf (map-elt client :message-queue) (copy-sequence messages))
    (setf (map-elt client :pending-requests) '())
    client))

(cl-defun acp-fakes--request-sender (&key client _request on-success on-failure)
  (let* ((request-id (1+ (map-elt client :request-id)))
         (message-queue (map-elt client :message-queue))
         (pending-requests (map-elt client :pending-requests)))
    (setf (map-elt client :request-id) request-id)
    (setf (map-elt pending-requests request-id) (list on-success on-failure))
    (setf (map-elt client :pending-requests) pending-requests)
    (let ((response-message (seq-find
                             (lambda (msg)
                               (and (eq (map-elt msg :kind) 'incoming)
                                    (equal (map-elt (map-elt msg :object) 'id)
                                           request-id)))
                             message-queue)))
      (when response-message
        (setf (map-elt client :message-queue)
              (seq-remove (lambda (msg)
                            (eq msg response-message))
                          message-queue))
        (let* ((response-obj (map-elt response-message :object))
               (callbacks (map-elt pending-requests request-id))
               (on-success (nth 0 callbacks))
               (on-failure (nth 1 callbacks))
               (result (map-elt response-obj 'result))
               (error (map-elt response-obj 'error)))
          (setf (map-elt client :pending-requests)
                (map-delete pending-requests request-id))
          (cond
           ((and result on-success)
            (funcall on-success result)
            result)
           ((and error on-failure)
            (funcall on-failure error)
            error)
           (t
            (error "No matching response found for request %s" request-id))))))))

(cl-defun acp-fakes--response-sender (&key _response)
  ;; Nothing left to do after sending.
  (ignore))

(cl-defun acp-fakes--request-resolver (&key _client _id)
  ;; Pending requests tracked in fake message-queue.
  (ignore))

(defun acp-fakes--test-fake-client ()
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

    ;; Test sending a request
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

(defun apc-fakes--traffic-objects (&optional buffer)
  (with-current-buffer (or buffer (get-buffer-create "*acp traffic*"))
    (save-excursion
      (goto-char (point-min))
      (let ((objects '()))
        (while (not (eobp))
          (let ((obj (get-text-property (point) 'acp-object)))
            (when obj
              (push obj objects)))
          (forward-line 1))
        (nreverse objects)))))

(defun apc-fakes--dump-traffic-objects (&optional buffer)
  (let ((objects (apc-fakes--traffic-objects)))
    (with-current-buffer (or buffer (get-buffer-create "*acp serialized traffic*"))
      (erase-buffer)
      (let ((print-circle t)
            (pp-use-max-width t))
        (pp objects (current-buffer)))
      (pop-to-buffer (current-buffer)))))
