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
;; acp-traffic enable viewing all ACP traffic passing through acp.el.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'hl-line)
(require 'map)

(defvar acp-traffic-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-x C-s") #'acp-traffic-save-to)
    (define-key map "n" #'acp-traffic-next-entry)
    (define-key map "p" #'acp-traffic-previous-entry)
    (define-key map [down] #'acp-traffic-next-entry)
    (define-key map [up] #'acp-traffic-previous-entry)
    (define-key map (kbd "RET") #'acp-traffic-display-entry)
    (define-key map [mouse-1] #'acp-traffic-display-entry)
    map)
  "Keymap for ACP-Traffic mode.")

(defun acp-traffic-save-to ()
  "Save traffic objects to a file."
  (interactive)
  (unless (derived-mode-p 'acp-traffic-mode)
    (user-error "Not in a traffic buffer"))
  (let* ((destination (read-file-name "Save traffic to: " nil "yolo.traffic"))
         (objects (acp-traffic--objects)))
    (when (or (not destination)
              (string-empty-p (string-trim destination)))
      (user-error "No destination file found"))
    (with-temp-file destination
      (erase-buffer)
      (let ((print-circle t))
        (pp objects (current-buffer))))
    (message "Saved %s" destination)))

(defun acp-traffic-read-file (traffic-file)
  "Read TRAFFIC-FILE into message objects."
  (with-temp-buffer
    (insert-file-contents traffic-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun acp-traffic-open-file ()
  "Select and open a traffic file."
  (interactive)
  (if-let* ((traffic-file (read-file-name "Open traffic file: " nil nil t))
            (messages (acp-traffic-read-file traffic-file))
            (buffer (get-buffer-create (format "*ACP traffic (%s)*" (file-name-nondirectory traffic-file)))))
      (progn
        (dolist (message messages)
          (acp-traffic-log-traffic :buffer buffer
                                   :direction (map-elt message :direction)
                                   :kind (map-elt message :kind)
                                   :message message))
        (pop-to-buffer buffer))
    messages
    (error "No session messages found")))

(defun acp-traffic-next-entry ()
  "Move to next traffic entry."
  (interactive)
  (forward-line)
  (acp-traffic--update-line-highlight)
  (acp-traffic-display-entry))

(defun acp-traffic-previous-entry ()
  "Move to previous traffic entry."
  (interactive)
  (forward-line -1)
  (acp-traffic--update-line-highlight)
  (acp-traffic-display-entry))

(defun acp-traffic-display-entry ()
  "Display expanded entry at point."
  (interactive)
  (if-let ((objects (list (get-text-property (point) 'acp-traffic-object))))
      (acp-traffic-display-objects objects)
    (error "Nothing to view")))

(defun acp-traffic-display-all-entries ()
  "Display all entries expanded."
  (interactive)
  (unless (derived-mode-p 'acp-traffic-mode)
    (user-error "Not in a traffic buffer"))
  (save-excursion
    (goto-char (point-min))
    (let ((objects '()))
      (while (not (eobp))
        (when-let ((object (get-text-property (point) 'acp-traffic-object)))
          (push object objects))
        (forward-line 1))
      (acp-traffic-display-objects (nreverse objects)))))

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

(defun acp-traffic--update-line-highlight ()
  "Update the line highlight overlay to current line."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'acp-traffic)
      (delete-overlay overlay)))
  (let ((overlay (make-overlay (line-beginning-position) (1+ (line-end-position)))))
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'acp-traffic t)))

(define-derived-mode acp-traffic-mode special-mode "ACP-traffic"
  "Major mode for ACP traffic monitoring."
  (setq buffer-read-only t)
  (use-local-map acp-traffic-mode-map)
  (acp-traffic--update-line-highlight))

(cl-defun acp-traffic-log-traffic (&key buffer direction kind message)
  "Log MESSAGE to BUFFER.
KIND may be `request', `response', or `notification'.
DIRECTION is either `incoming' or `outgoing', OBJECT is the parsed object."
  (save-excursion
    (let ((inhibit-read-only t))
      (with-current-buffer buffer
        (goto-char (point-max))
        (let* ((object (map-elt message :object))
               (timestamp (format-time-string "%T.%3N"))
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
                                (:object . ,object))))
          (add-text-properties 0 (length line-text)
                               `(acp-traffic-object ,traffic-entry)
                               line-text)
          (insert line-text))
        ;; Keep buffer size manageable (last 1000 lines)
        (when (> (count-lines (point-min) (point-max)) 1000)
          (goto-char (point-min))
          (forward-line 100)
          (delete-region (point-min) (point)))))))

(defun acp-traffic--objects ()
  "Extract all the traffic objects from current traffic buffer."
  (unless (derived-mode-p 'acp-traffic-mode)
    (user-error "Not in a traffic buffer"))
  (save-excursion
    (goto-char (point-min))
    (let ((objects '()))
      (while (not (eobp))
        (let ((obj (get-text-property (point) 'acp-traffic-object)))
          (when obj
            (push obj objects)))
        (forward-line 1))
      (nreverse objects))))

;;;; Full traffic entry display

(defvar acp-traffic-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" #'acp-traffic-entry-next)
    (define-key map "p" #'acp-traffic-entry-previous)
    (define-key map [down] #'acp-traffic-entry-next)
    (define-key map [up] #'acp-traffic-entry-previous)
    map)
  "Keymap for ACP-Traffic entry mode.")

(define-derived-mode acp-traffic-entry-mode special-mode "ACP-traffic-entry"
  "Major mode for ACP traffic entry display."
  (setq buffer-read-only t)
  (use-local-map acp-traffic-entry-mode-map))

(defvar-local acp-traffic-entry--traffic-buffer nil
  "Buffer-local variable pointing to the associated traffic buffer.")

(defun acp-traffic-entry-next ()
  "Move to next traffic entry in the traffic buffer."
  (interactive)
  (when acp-traffic-entry--traffic-buffer
    (if-let ((window (get-buffer-window acp-traffic-entry--traffic-buffer)))
        (with-selected-window window
          (acp-traffic-next-entry)
          (acp-traffic--update-line-highlight))
      (acp-traffic-next-entry)
      (acp-traffic--update-line-highlight))))

(defun acp-traffic-entry-previous ()
  "Move to previous traffic entry in the traffic buffer."
  (interactive)
  (when acp-traffic-entry--traffic-buffer
    (if-let ((window (get-buffer-window acp-traffic-entry--traffic-buffer)))
        (with-selected-window window
          (acp-traffic-previous-entry)
          (acp-traffic--update-line-highlight))
      (acp-traffic-previous-entry)
      (acp-traffic--update-line-highlight))))

(defun acp-traffic-display-objects (objects)
  "Display OBJECTS."
  (let ((traffic-buffer (current-buffer))
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*ACP traffic entry*")
      (erase-buffer)
      (dolist (object objects)
        (acp-traffic-display-objects-helper object 0)
        (insert "\n"))
      (acp-traffic-entry-mode)
      (setq acp-traffic-entry--traffic-buffer traffic-buffer)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun acp-traffic-display-objects-helper (object indent)
  "Display OBJECT with INDENT."
  (cond
   ((and (listp object) (consp (car object)) (symbolp (caar object)))
    (let ((max-key-width (acp-traffic-display-max-key-width object)))
      (map-do (lambda (key value)
                (insert (make-string indent ?\s))
                (let ((label (symbol-name key)))
                  (insert (propertize label 'face 'font-lock-variable-name-face))
                  (cond
                   ((or (and (listp value) (listp (car value)))
                        (vectorp value))
                    (insert "\n")
                    (acp-traffic-display-objects-helper value (+ indent max-key-width 1)))
                   (t
                    (insert (make-string (- max-key-width (length label)) ?\s))
                    (insert (format " %s\n" (acp-traffic-display-format-value value)))))))
              object)))
   ((listp object)
    (dolist (item object)
      (acp-traffic-display-objects-helper item indent)))
   ((vectorp object)
    (seq-do-indexed (lambda (item idx)
                      (acp-traffic-display-objects-helper item indent)
                      (when (< idx (1- (length object)))
                        (insert "\n")))
                    object))))

(defun acp-traffic-display-max-key-width (alist)
  "Return longest key width in ALIST."
  (let ((max-width 0))
    (map-do (lambda (key _value)
              (let ((key-len (length (symbol-name key))))
                (when (> key-len max-width)
                  (setq max-width key-len))))
            alist)
    max-width))

(defun acp-traffic-display-format-value (value)
  "Format display of VALUE."
  (cond
   ((stringp value)
    ;; TODO: Should it cap string length?
    (if nil ;;(> (length value) 100)
        (concat (substring value 0 100) "...")
      value))
   ((numberp value) (format "%s" value))
   ((eq value :false) ":false")
   ((eq value :null) ":null")
   ((eq value t) "t")
   ((symbolp value) (symbol-name value))
   (t "")))

(provide 'acp-traffic)

;;; acp-traffic.el ends here
