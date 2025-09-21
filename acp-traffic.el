;;; acp.el --- A mode to log and display ACP traffic -*- lexical-binding: t; -*-

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
;; acp.el implements ACP (Agent Client Protocol) as per spec
;; https://agentclientprotocol.com
;;
;; Note: This package is in the very early stage and is likely
;; incomplete or may have some rough edges.
;;
;; Report issues at https://github.com/xenodium/acp.el/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;

;;; Code:

(defvar acp-traffic-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-x C-s") #'acp-traffic-save)
    (define-key map "n" #'acp-traffic-next-entry)
    (define-key map "p" #'acp-traffic-previous-entry)
    map)
  "Keymap for ACP-Traffic mode.")

(defun acp-traffic-save ()
  "Handle save command in ACP-Traffic mode."
  (interactive)
  (error "FIXME"))

(defun acp-traffic-next-entry ()
  "Move to next traffic entry."
  (interactive)
  (forward-line))

(defun acp-traffic-previous-entry ()
  "Move to previous traffic entry."
  (interactive)
  (forward-line -1))

(cl-defun acp-traffic-get-buffer (&key named)
  "Get or create a buffer for ACP traffic.

NAMED is required name to create buffer if needed."
  (unless named
    (error ":named is required"))
  (if (get-buffer named)
      (get-buffer named)
    (with-current-buffer (get-buffer-create named)
      (acp-traffic-mode)
      (current-buffer))))

(define-derived-mode acp-traffic-mode special-mode "ACP-traffic"
  "Major mode for ACP traffic monitoring."
  (setq buffer-read-only t)
  (use-local-map acp-traffic-mode-map))

(cl-defun acp-traffic-log-traffic (&key buffer direction kind message)
  "Log MESSAGE to BUFFER.
KIND may be `request', `response', or `notification'.
DIRECTION is either `incoming' or `outgoing', OBJECT is the parsed object."
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
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
                                (propertize (if (eq direction 'incoming)
                                                "←"
                                              "→")
                                            'face (if (eq direction 'incoming)
                                                      'success
                                                    'error))
                                kind
                                (propertize method-info 'face font-lock-function-name-face)))
             (traffic-entry `((:direction . ,direction)
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
                               acp-traffic-object ,traffic-entry)
                             line-text)
        (insert line-text)))
    ;; Keep buffer size manageable (last 1000 lines)
    (when (> (count-lines (point-min) (point-max)) 1000)
      (goto-char (point-min))
      (forward-line 100)
      (delete-region (point-min) (point)))))

(provide 'acp-traffic)

;;; acp-traffic.el ends here
