;;; wave-client-websocket.el --- Methods to communicate with the Wave server

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

;; This code represents a replaceable backend layer for the Emacs wave
;; mode.  It is responsible for communicating with the Wave server.
;;
;; This backend is using the new FedOne websocket-based client/server
;; protocol.
;;


;;; Code:
(eval-and-compile
  (require 'cl)
  (require 'json)
  (require 'wave-data)
  (require 'wave-util)
  (require 'websocket)
  (require 'url))

(defvar wave-client-ws-channel-num 0
  "Incrementing counter for channel numbers.")

(defvar wave-client-ws-url "ws://localhost:9001/socket.io/websocket"
  "The URL for the websocket connection.")

(defvar wave-client-ws-websocket nil
  "The websocket supporting this connection.")

(defvar wave-client-ws-channel-callbacks nil
  "Hash table of channel numbers to callbacks.")

(defvar wave-client-ws-wave-channels nil
  "Hash table of wave ids to channel numbers.")

(defvar wave-client-ws-wavelet-states nil
  "Nested hash table of wave id -> wavelet id -> wave-wavelet object.")

(defvar wave-client-ws-unfinished-packets ""
  "The text of json that span packets.")

(defvar wave-client-ws-wave-callbacks (make-hash-table :test 'equal)
  "Wave id to callback.")

(defun wave-client-ws-reset ()
  (interactive)
  (unwind-protect
      (when wave-client-ws-websocket
        (websocket-close wave-client-ws-websocket))
    (setq wave-client-ws-websocket nil
        wave-client-ws-wave-channels nil
        wave-client-ws-channel-callbacks nil
        wave-client-ws-wavelet-states nil
        wave-client-ws-wave-callbacks nil
        wave-client-ws-unfinished-packets nil)))

(defun wave-client-ws-connect (url)
  "Open a websocket connection to URL."
  (setq wave-client-ws-wave-channels (make-hash-table :test 'equal)
        wave-client-ws-channel-callbacks (make-hash-table)
        wave-client-ws-wavelet-states (make-hash-table :test 'equal))
  (setq wave-client-ws-websocket
        (let ((websocket-use-v75 t))
          (websocket-open url 'wave-client-ws-filter 'wave-client-ws-onclose))))

(defun wave-client-ws-fixup-control-codes (text)
  "Fix issues with JSON control code detection"
  (replace-regexp-in-string "\\\\\\([^\\\"]\\)" "\\\\\\\\\\\\\\1" text))

(defun wave-client-ws-filter (packet)
  "Filter OUTPUT from our websocket.  As websocket responses come
back, parse them and call the appropriate callbacks."
  (wave-debug "Received packet: %s" packet)
  ;; TODO(ohler): redo this properly
  (let ((json-object-type 'plist)
        (json-key-type nil))
    (wave-debug "Received packet: %s" packet)
    (if (not (string-match "}$" packet))
        (setq wave-client-ws-unfinished-packets
              (concat wave-client-ws-unfinished-packets packet))
      (let* ((response (json-read-from-string
                        (wave-client-ws-fixup-control-codes
                         (concat wave-client-ws-unfinished-packets
                                 packet))))
             (channel-number (plist-get response :sequenceNumber))
             (message-type (plist-get response :messageType))
             (message (json-read-from-string (plist-get response
                                                        :messageJson))))
        (let ((callback (gethash channel-number
                                 wave-client-ws-channel-callbacks)))
          (setq wave-client-ws-unfinished-packets "")
          (if callback
              (funcall callback message-type message)
            (message
             "No callback for channel number %s, discarding message %s %S"
             channel-number message-type message)))))))

(defun wave-client-ws-send-raw (text)
  "Send the raw TEXT as a websocket packet."
  (wave-debug "Sending to websocket: %s" text)
  (websocket-send wave-client-ws-websocket text))

(defun wave-client-ws-json-fixup (text)
  "Fix some escapification issues that cause problems."
  (replace-regexp-in-string
   "\\\\\\\\" "\\\\"
   (replace-regexp-in-string
    "\\\\+/" "/"
    (replace-regexp-in-string "null" "{}"
                              text))))

(defun wave-client-ws-cntrl-fixup (text)
  (replace-regexp-in-string "\\([[:cntrl:]]\\)"
                            (lambda (c)
                              (format "\\\\%03o" (string-to-char c)))
                            text))

(defun wave-client-ws-strip-domain (id)
  "Strip the domain from ID."
  (replace-regexp-in-string (format "%s!" (wave-client-domain))
                            "" id))

(defun wave-client-ws-send (type obj channel-number)
  (wave-client-ws-send-raw
   (wave-client-ws-json-fixup
    (json-encode
     `(:version 0
                :sequenceNumber ,channel-number
                :messageType ,type
                :messageJson ,(json-encode obj))))))

(defun wave-client-ws-open (obj callback)
  "Sends the req OBJ to the server and opens a channel for responses.

CALLBACK should be a two-argument function accepting a response
type and response.  It will be called once for every response
that the server sends on the channel.

Returns the channel number."
  (let ((channel-number (incf wave-client-ws-channel-num)))
    (puthash channel-number callback wave-client-ws-channel-callbacks)
    (wave-client-ws-send "ProtocolOpenRequest" obj channel-number)
    channel-number))

(defun wave-client-ws-onclose ()
  "Callback called when the websocket closes.  Reconnect and
re-establish all the listeners needed to keep our buffers
updated."
  ;; Reconnect.
  (wave-client-ws-connect wave-client-ws-url)
  (when wave-client-ws-wave-callbacks
    (maphash (lambda (wave-id callback)
               (wave-ws-get-wave wave-id callback))
             wave-client-ws-wave-callbacks)))

(defun wave-client-ws-submit (obj)
  "Submits OBJ to the server."
  (let ((channel-number (incf wave-client-ws-channel-num)))
    (wave-client-ws-send "ProtocolSubmitRequest"
                         obj channel-number)))

;; Two possible formats:
;; wave://waveletdomain/wavelocalid/waveletlocalid
;; wave://waveletdomain/wavedomain!wavelocalid/waveletlocalid
(defun wave-client-ws-parse-wavelet-name (url)
  (assert (string-match "\\`wave://\\(.*\\)\\'" url))
  (let* ((x (match-string 1 url))
         (parts (split-string x "/")))
    (assert (eql (length parts) 3))
    (let (wavedomain
          wavelocalid
          waveletdomain
          waveletlocalid)
      (if (not (position ?! (second parts)))
          (setq wavedomain (first parts)
                wavelocalid (second parts)
                waveletdomain (first parts)
                waveletlocalid (third parts))
        (let ((split (split-string (second parts) "!")))
          (assert (eql (length split) 2))
          (setq wavedomain (first split)
                wavelocalid (second split)
                waveletdomain (first parts)
                waveletlocalid (third parts))))
      (let ((wavelet-name (cons (concat wavedomain "!" wavelocalid)
                                (concat waveletdomain "!" waveletlocalid))))
        wavelet-name))))

(defun wave-client-ws-parse-delta (raw-delta)
  ;; ProtocolWaveletDelta
 (let* ((operation (plist-get raw-delta :3))
        (author (plist-get raw-delta :2))
        (hashed-version (plist-get raw-delta :1))
        (history-hash (plist-get hashed-version :2))
        (version (plist-get hashed-version :1)))
   (wave-make-delta :author author
                    :pre-version (cons version history-hash)
                    :post-version (cons (+ version (length operation)) 0)
                    :timestamp 0
                    :ops (map 'vector 'wave-client-ws-parse-op
                              operation))))

(defun wave-client-ws-wavelet-callback (type response)
  (wave-debug "Received response %s" response)
  (ecase (intern-soft type)
    (ProtocolWaveletUpdate
     ;; Using ProtocolWaveletUpdate
     (let* ((wavelet-name (wave-client-ws-parse-wavelet-name
                           (plist-get response :1)))
            (wave-id (car wavelet-name))
            (wavelet-id (cdr wavelet-name))
            (resulting-version (plist-get response :4))
            (deltas (map 'list #'wave-client-ws-parse-delta
                         (plist-get response :2))))
       (assert deltas)
       (let ((wave (gethash wave-id wave-client-ws-wavelet-states)))
         (when (null wave)
           (setq wave (make-hash-table :test 'equal))
           (puthash wave-id wave wave-client-ws-wavelet-states))
         (let ((wavelet (gethash wavelet-id wave)))
           (when (zerop (car (wave-delta-pre-version (first deltas))))
             ;; The first delta, or the server rewrote the history.
             (setq wavelet nil))
           (when (null wavelet)
             (setq wavelet (wave-make-wavelet
                            :wavelet-name wavelet-name
                            :creator (wave-delta-author (first deltas))
                            :creation-time (wave-delta-timestamp (first deltas))))
             (puthash wavelet-id wavelet wave))
           (dolist (delta deltas)
             (wave-apply-delta wavelet delta))
           (setf (wave-wavelet-version wavelet)
                 (cons
                  ;; ProtocolHashedVersion
                  (plist-get resulting-version :1)
                  (plist-get resulting-version :2)))
           (let ((callback (gethash wave-id wave-client-ws-wave-callbacks)))
             (when callback
               (wave-debug "Sending callback for %s at time %f"
                           wave-id (float-time))
               (funcall callback
                        (wave-ws-return-wave wave-id))))))))
    ((nil)
     (error "Unknown type: %S; response=%S" type response))))

(defconst wave-client-ws-indexwave-wave-id "indexwave!indexwave")

(defun wave-client-ws-open-wave-channel (wave-id)
  ;; ProtocolOpenRequest
  (wave-client-ws-open `(:1 ,(concat wave-client-user
                                     "@" (wave-client-domain))
                            :2 ,wave-id
                            :3 (""))
                       'wave-client-ws-wavelet-callback))

(defun wave-client-ws-ensure-wave-channel-open (wave-id)
  "Returns the channel number of the channel of the given wave."
  (or (gethash wave-id wave-client-ws-wave-channels)
      (let ((channel (wave-client-ws-open-wave-channel wave-id)))
        (puthash wave-id channel wave-client-ws-wave-channels)
        channel)))

(defun wave-client-ws-parse-component (raw-component)
  (destructuring-bind (type arg) raw-component
    (ecase type
      ;; TODO(ahyatt) There's no reason not to use structs here,
      ;; we just need to modify this and wave-apply-doc-op.

      ;; ProtocolDocumentOperation.Component
      (:1 (list :annotation_boundary arg))
      (:2 (list :characters arg))
      (:3 (list :element_start
                (list :type (plist-get arg :1)
                      :attribute
                      (apply 'vector
                             (mapcar (lambda (kv-pair)
                                       (list
                                        :key (plist-get kv-pair :1)
                                        :value (plist-get kv-pair :2)))
                                     (plist-get arg :2))))))
      (:4 '(:element_end t))
      (:5 (list :retain_item_count arg))
      (:6 (list :delete_characters arg))
      (:7 (list :delete_element_start arg))  ;; unimplemented
      (:8 (list :delete_element_end t))      ;; unimplemented
      (:9 (list :replace_attributes arg))    ;; unimplemented
      (:10 (list :update_attributes arg))))) ;; unimplemented

(defun wave-client-ws-parse-op (raw-op)
  (destructuring-bind (type arg) raw-op
    (ecase type
      ;; ProtocolWaveletOperation
      (:3
       ;; ProtocolWaveletOperation.MutateDocument
       ;; ProtocolDocumentOperation
       (wave-make-doc-op :doc-id (intern (plist-get arg :1))
                         :components
                         (apply 'vector
                                (mapcar #'wave-client-ws-parse-component
                                        (plist-get (plist-get arg :2) :1)))))
      (:1
       (wave-make-add-participant :address arg))
      (:2
       (wave-make-remove-participant :address arg))
      (:4
       (assert (eql arg t) t)
       (wave-make-no-op)))))

(defun wave-client-ws-translate-inbox (wavelets)
  (let ((plists
         (loop for wavelet in wavelets
               collect
               `(:id ,(cdr (wave-wavelet-wavelet-name wavelet))
                     :digest
                     ,(let ((doc (gethash 'digest (wave-wavelet-docs wavelet))))
                        (if (null doc)
                            ""
                          ;; TODO(ohler): extract character content
                          ;; from document properly
                          (format "%S" (wave-doc-content doc))))
                     :unread 0             ; we don't know
                     :creator "author"     ; we don't know
                     ))))
    (sort* plists #'string< :key (lambda (plist) (plist-get plist :id)))))

(defun wave-ws-component-to-obj (component)
  ;; using ProtocolDocumentOperation
  (etypecase
   component
   (wave-text
    (list :2 (wave-client-ws-cntrl-fixup
                       (wave-text-text component))))
   (wave-element-start
    (list :3
          ;; ElementStart
          (append (list :1 (wave-element-start-type component))
                  (when (wave-element-start-attributes component)
                    (list :2
                          (apply
                           'vector
                           (mapcar (lambda (p)
                                     ;; KeyValuePair
                                     (list :1 (wave-key-value-pair-key p)
                                           :2 (wave-key-value-pair-value p)))
                                   (wave-element-start-attributes component))))))))
   (wave-element-end
    (list :4 t))
   (wave-retain-item-count
    (list :5 (wave-retain-item-count-num component)))))

(defun wave-ws-op-to-obj (op)
  ;; using ProtocolWaveletOperation
  (etypecase
   op
   (wave-add-participant
    (list :1
          (wave-add-participant-address op)))
   (wave-remove-participant
    (list :2
          (wave-remove-participant-address op)))
   (wave-blip-submit
    (list :3
          ;; ProtocolWaveletOperation.MutateDocument
          (list :1 (wave-blip-submit-blip-id op)
                :2 '())))
   (wave-doc-op
    (list :3
           (list :1 (wave-doc-op-doc-id op)
                 :2
                 (list :1
                       (apply 'vector
                              (mapcar (lambda (c)
                                        (wave-ws-component-to-obj c))
                                      (wave-doc-op-components op)))))))))

(defun wave-ws-wavelet-name-to-url (wavelet-name)
  "Turn WAVELET-NAME to a wave:// url."
  (format "wave://%s/%s/%s" wave-client-domain
          (wave-client-ws-strip-domain
           (car wavelet-name))
          (wave-client-ws-strip-domain
           (cdr wavelet-name))))

(defun wave-ws-delta-to-obj (delta wavelet-name)
  ;; ProtocolSubmitRequest
  (list :1 (wave-ws-wavelet-name-to-url wavelet-name)
        :2
        ;; ProtocolWaveletDelta
        (list :1
              ;; ProtocolHashedVersion
              (list :1 (car (wave-delta-pre-version delta))
                    :2 (wave-client-ws-cntrl-fixup
                        (cdr (wave-delta-pre-version delta))))
              :2 (wave-delta-author delta)
              :3 (apply 'vector
                        (mapcar (lambda (op)
                                  (wave-ws-op-to-obj op))
                                (wave-delta-ops delta))))))

(defun wave-ws-return-wave (wave-id)
  (let ((wave (gethash wave-id wave-client-ws-wavelet-states)))
    (if (null wave)
        nil
      (assert (plusp (hash-table-count wave)))
      (let ((wavelets (list)))
        (maphash (lambda (key wavelet)
                   (push wavelet wavelets))
                 wave)
        (nreverse wavelets)))))

;;; Functions for the Wave mode to use:

(defun wave-ws-get-wave (wave-id callback)
  (unless (websocket-openp wave-client-ws-websocket)
    (wave-client-ws-connect wave-client-ws-url))
  (wave-client-ws-ensure-wave-channel-open wave-id)
  ;; Wait an arbitrary amount of time for some data to arrive.
  (accept-process-output nil 1)
  (puthash wave-id callback wave-client-ws-wave-callbacks)
  (wave-ws-return-wave wave-id))

(defun wave-ws-get-inbox (callback)
  (let ((indexwave (wave-ws-get-wave wave-client-ws-indexwave-wave-id
                                     callback)))
    (let ((inbox (wave-client-ws-translate-inbox indexwave)))
      inbox)))

(defun wave-ws-send-delta (delta wavelet-name)
  (wave-client-ws-submit (wave-ws-delta-to-obj delta wavelet-name)))

(provide 'wave-client-websocket)

;;; wave-client-websocket.el ends here
