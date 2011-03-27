;;; wave-util.el --- Common utility methods for the project.

;; Copyright (c) 2010 Andrew Hyatt
;;
;; Author: Andrew Hyatt <ahyatt at gmail dot com>
;; Maintainer: Andrew Hyatt <ahyatt at gmail dot com>
;;
;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements. See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership. The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License. You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations
;; under the License.


;;; Commentary:
;;
;; Utility functions used by the rest of the project code.  This file
;; should have no other dependencies.

;;; Code:
(defvar wave-debug t
  "Whether to debug or not.
If we do, debug output is be sent to wave-client-debug-buffer")

(defconst wave-debug-buffer "*Wave Client Debug Buffer*"
  "Name of the buffer wear debug output is sent to.")

(defun wave-debug (str &rest args)
  "Send STR to the `wave-debug-buffer', with format args ARGS."
  (when wave-debug
    (save-excursion
      (set-buffer
       (get-buffer-create wave-debug-buffer))
      (goto-char (point-max))
      (insert (apply 'format (append (list str) args)))
      (insert "\n"))))

(defun wave-random-b64-string (digits)
  "Return a random base 64 string of DIGITS length.
Wave doesn't use +, though, so we substitute - instead."
  (let ((s '()))
    (dotimes (v digits)
      (setq s (cons (random 64) s)))
    (replace-regexp-in-string
     "+" "-" (substring (base64-encode-string (apply 'string s)) 0 digits))))

(defun wave-expand-raw (raw)
  "Canonicalize the operation data RAW.
The wave data has a few rules that need following: annotations
are not counted as operations, and each character of a string is
counted separately.  This should be idempotent."
  (let ((expanded '()))
    (dolist (e raw)
      (cond ((stringp e)
             (dolist (c (string-to-sequence e 'list))
               (setq expanded (cons (char-to-string c) expanded))))
            (t
             (unless (and (listp e)
                          (eq (car e) '@boundary))
               (setq expanded (cons e expanded))))))
    (nreverse expanded)))

(defun wave-id-suffix (wave-id)
  "Return the part after the ! in WAVE-ID.
For example, example.com!conv+root becomes conv+root."
  (substring wave-id (+ 1 (or (string-match "!" wave-id) -1))))

(defmacro defproto (name &rest struct-elems)
  "Replacement for defstruct, allows for proto field number
  mappings"
  (let ((struct-elems (if (stringp (car struct-elems))
                          (cdr struct-elems)
                        struct-elems))
        (struct-name (gensym name)))
    `(progn (defstruct ,struct-name
              ,@(mapcar (lambda (property) (list (car property) nil :read-only t))
                        struct-elems))
            (defun ,(intern (concat (symbol-name name) "-to-proto")) (item)
              (let ((plist '()))
                (dolist (field-def (quote ,struct-elems))
                  (let ((field-val
                         (funcall
                          (intern
                           (concat (symbol-name (quote ,struct-name))
                                   "-"
                                   (symbol-name (car field-def)))) item)))
                    (when field-val
                      (setq plist
                            (plist-put plist (cadr field-def) field-val)))))
                plist))
            (defun ,(intern (concat (symbol-name name) "-proto"))
              (&rest fields)
              (,(intern (concat (symbol-name name) "-to-proto"))
               (apply (quote
                       ,(intern (concat "make-" (symbol-name struct-name))))
                      fields))))))

(provide 'wave-util)

;;; wave-util.el ends here
