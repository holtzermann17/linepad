;;; wave-data.el --- Wave data structures and operations on them

;; Copyright (c) 2010 Christian Ohler
;;
;; Author: Christian Ohler <ohler at google dot com>
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

;; TODO(ohler): add commentary


;; Intended to be mutated.
(defstruct (wave-wavelet (:constructor wave-make-wavelet))
  (wavelet-name (assert nil) :read-only t)
  (creator (assert nil) :read-only t)
  (creation-time (assert nil) :read-only t)
  ;; A distinct version.
  (version (cons 0 0))
  (last-modified-time nil)
  (participants (list))
  ;; The contents of the hash table are read-write, the reference to
  ;; it is read-only.
  (docs (make-hash-table) :read-only t))

;; Intended to be mutated.
(defstruct (wave-doc (:constructor wave-make-doc))
  ;; An interned symbol.
  (doc-id (assert nil) :read-only t)
  (contributors (list))
  (last-modified-version nil)
  (last-modified-time nil)
  ;; A document initialization.
  (content '()))

;; Intended to be immutable.
(defstruct (wave-delta (:constructor wave-make-delta))
  (author (assert nil) :read-only t)
  ;; A distinct version.
  (pre-version (assert nil) :read-only t)
  ;; A distinct version.  Filled in only on the server, nil for deltas that we
  ;; create
  (post-version nil :read-only t)
  ;; Filled in on the server, nil for deltas that we create
  (timestamp nil :read-only t)
  ;; A list of operations
  (ops (assert nil) :read-only t))

(defstruct (wave-op (:constructor wave-make-op))
  )

;; Intended to be immutable.
(defstruct (wave-add-participant (:include wave-op)
                                 (:constructor wave-make-add-participant))
  (address (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-remove-participant (:include wave-op)
                                    (:constructor wave-make-remove-participant))
  (address (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-blip-submit (:include wave-op)
                             (:constructor wave-make-blip-submit))
  (blip-id (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-no-op (:include wave-op)
                       (:constructor wave-make-no-op))
  )

;; Intended to be immutable.
(defstruct (wave-doc-op (:include wave-op)
                        (:constructor wave-make-doc-op))
  ;; An interned symbol.
  (doc-id (assert nil) :read-only t)
  ;; The operation components, a vector.
  (components (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-component (:constructor wave-make-component)))

;; Intended to be immutable.
(defstruct (wave-text (:include wave-component)
                      (:constructor wave-make-text))
  ;; A string
  (text (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-retain-item-count
            (:include wave-component)
            (:constructor wave-make-retain-item-count))
  ;; An integer
  (num (assert nil) :read-only t))

;; Intended to be immutable.
(defstruct (wave-element-start (:include wave-component)
                               (:constructor wave-make-element-start))
  ;; A string of a certain well-defined set of types ("blip", etc.)
  (type (assert nil) :read-only t)
  ;; A list of wave-key-value-pairs
  (attributes nil :read-only t))

;; Intended to be immutable.
(defstruct (wave-element-end
            (:include wave-component)
            (:constructor wave-make-element-end)))

;; Intended to be immutable.
(defstruct (wave-key-value-pair (:constructor wave-make-key-value-pair))
  (key (assert nil) :read-only t)
  (value (assert nil) :read-only t))

(defun wave-make-initial-wavelet (wavelet-name creator)
  (wave-make-wavelet
   :wavelet-name wavelet-name
   :creator creator))

(defun wave-apply-delta (wavelet delta)
  "Apply DELTA to WAVELET.  Operates by modifying WAVELET."
  (wave-debug "Received update for wavelet %s version %d"
              (wave-wavelet-wavelet-name wavelet)
              (car (wave-wavelet-version wavelet)))
  (when (equal (car (wave-wavelet-version wavelet))
               (car (wave-delta-pre-version delta)))
    (loop
     with timestamp = (wave-delta-timestamp delta)
     with delta-author = (wave-delta-author delta)
     for op across (wave-delta-ops delta)
     for post-version from (1+ (car (wave-wavelet-version wavelet))) do
     (etypecase op
       (wave-add-participant
        ;; The participant list is ordered, we have to add at the end.
        (setf (wave-wavelet-participants wavelet)
              (append (wave-wavelet-participants wavelet)
                      (list (wave-add-participant-address op)))))
       (wave-remove-participant
        (setf (wave-wavelet-participants wavelet)
              (remove (wave-remove-participant-address op)
                      (wave-wavelet-participants wavelet))))
       (wave-no-op
        ;; do nothing
        )
       (wave-doc-op
        (let* ((doc-id (wave-doc-op-doc-id op))
               (doc (gethash doc-id (wave-wavelet-docs wavelet))))
          (when (null doc)
            (setq doc (wave-make-doc :doc-id doc-id))
            (puthash doc-id doc (wave-wavelet-docs wavelet)))
          ;; TODO(ohler): implement worthiness check
          (setf (wave-doc-content doc) (wave-apply-doc-op
                                        (wave-doc-content doc)
                                        (wave-doc-op-components op))
                (wave-doc-contributors doc) (adjoin
                                             delta-author
                                             (wave-doc-contributors doc)
                                             :test 'equal)
                (wave-doc-last-modified-version doc) post-version
                (wave-doc-last-modified-time doc) timestamp))))
     finally
     (assert (eql (1- post-version) (car (wave-delta-post-version delta))) t))
    (setf (wave-wavelet-version wavelet) (wave-delta-post-version delta)
          (wave-wavelet-last-modified-time wavelet) (wave-delta-timestamp delta)))
  wavelet)

(defun wave-compact-element-from-verbose (element)
  "Converts representation of attributes.

Turns (:type \"foo\" :attribute [(:key \"xx\" :value \"yy\") ...])
into (foo (xx \"yy\" ...))."
  (destructuring-bind (&key type attribute) element
    (list (intern type)
          (loop for attr across attribute
                nconc (destructuring-bind (&key key value) attr
                        (list (intern key) value))))))

(defun wave-normalize-doc-init (init)
  ;; This may also take over some annotation boundary fixups if that
  ;; simplifies wave-apply-doc-op.
  (let ((accu (list)))
    (while (and init (cdr init))
      (cond ((and (stringp (car init)) (stringp (cadr init)))
             ;; This is not very efficient.  Let's see if this becomes
             ;; a problem.
             (push (concat (pop init) (pop init)) init))
            (t (push (pop init) accu))))
    (when init (push (pop init) accu))
    (assert (endp init))
    (nreverse accu)))

(defun wave-apply-doc-op (init op-components)
  "Returns an initialization that corresponds to INIT with OP-COMPONENTS applied.

This may be destructive on the top-level structure of init, but nothing else.
The resulting initialization may share structure with OP-COMPONENTS.
INIT is a document initialization as described on the wiki.
OP-COMPONENTS should be a vector of entries like
\(:characters \"xyz\")
\(:delete_characters \"xyz\")
\(:retain_item_count 8)
\(:element_start (:type \"foo\" :attribute [(:key \"xx\" :value \"yy\")]))
\(:element_end t)
\(:annotation_boundary ...)  ; ignored for now
\(:delete_element_start (:type \"foo\" :attribute [(:key \"xx\" :value \"yy\")]))
\(:delete_element_end t)
...update/replace attributes...
TODO(ohler): Introduce a better data format for this as soon as the
browser channel backend gives us operations.  For now, only
the websocket backend does, so we use its format verbatim.
"
  ;; accu accumulates the components of the resulting initialization.
  ;; We only keep the remaining unprocessed part of the original
  ;; document in init.
  (let ((accu (list)))
    (loop for component across op-components
          for (type arg) = component do
          ;;(message "component=%S, init=%S, accu=%S" component init (reverse accu))
          (ecase type
            (:characters (push arg accu))
            (:delete_characters
             (let ((chars-to-delete (length arg)))
               (while (plusp chars-to-delete)
                 (let ((here (pop init)))
                   (cond ((stringp here)
                          (if (>= chars-to-delete (length here))
                              (decf chars-to-delete (length here))
                            (push (substring here chars-to-delete) init)
                            (setq chars-to-delete 0)))
                         ((and (listp here) (eql (first here) '@boundary))
                          ;; TODO(ohler): implement this correctly
                          (push here accu))
                         (t
                          (error "Attempt to use delete_characters on non-characters: %S, %S"
                                 arg here)))))))
            (:retain_item_count
             (while (plusp arg)
               (let ((here (pop init)))
                 (cond ((stringp here)
                        (if (>= arg (length here))
                            (progn
                              (push here accu)
                              (decf arg (length here)))
                          (push (substring here arg) init)
                          (push (substring here 0 arg) accu)
                          (setq arg 0)))
                       (t
                        (push here accu)
                        (decf arg 1))))))
            (:element_start
             (push (wave-compact-element-from-verbose arg) accu))
            (:element_end
             (push 'end accu))
            (:delete_element_start
             ;; TODO(ohler): add error checks
             (pop init))
            (:delete_element_end
             ;; TODO(ohler): add error checks
             (pop init))
            (:annotation_boundary
             ;; TODO(ohler): implement
             )))
    (assert (endp init) t "Operation did not traverse entire document, remainder: %S")
    (wave-normalize-doc-init (nreverse accu))))

(provide 'wave-data)
