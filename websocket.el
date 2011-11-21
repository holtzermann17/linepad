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
;; This implements two version of the websocket protocol, the older
;; v75 protocol:
;; http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-75
;;
;; By default, we use the newer v76 protocol:
;; http://www.whatwg.org/specs/web-socket-protocol/

(require 'url-parse)
(require 'calc)
(require 'cl)

;;; Code:
(defstruct websocket
  (conn (assert nil) :read-only t)
  (filter (assert nil) :read-only t)
  (close-callback (assert nil) :read-only t)
  (url (assert nil) :read-only t)
  (inflight-packet nil)
  (v75 (assert nil) :read-only t))

(defvar websocket-use-v75 nil
  "Set to true if to use the older v75 protocol.
Best set in a LET statement around the `websocket-open' reply.")

(defvar websocket-debug t
  "Set to true to output debugging info to a per-websocket buffer.
The buffer is ` *websocket URL debug*' where URL is the
URL of the connection.")

(defconst websocket-keylen 22)

(defun websocket-genbytes ()
  "Generate bytes used at the end of the handshake."
  (let ((s '()))
    (dotimes (v 8)
      (push (random 256) s))
    (apply 'string s)))

(defun websocket-random-insert (str-to-insert target-str)
  "Insert STR-TO-INSERT at a random position in TARGET-STR."
  (let ((r (+ 1 (random (- (length target-str) 2)))))
    (concat (substring target-str 0 r) str-to-insert
            (substring target-str r))))

(defun websocket-genkey ()
  (let ((avail (mapcar 'char-to-string 
                       (append "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz" nil)))
        (result ""))
    (dotimes (i 22)
      (setq result (concat (nth (random 61) avail)
                           result)))
    (concat result "==")))

;;; This function gets called by someone over and over again.
;; Need to run a trace on it and try to figure out why.

;; original `websocket-open' with a few changes
(defun websocket-open (url filter &optional close-callback)
  "Open a websocket connection to URL.
Websocket packets are sent as the only argument to FILTER, and if
the connection is closed, then CLOSE-CALLBACK is called."
  (let* ((name (format "websocket to %s" url))
         (url-struct (url-generic-parse-url url))
         (key (websocket-genkey))
         (bytes (websocket-genbytes))
         (buf-name (format " *%s*" name))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (conn (if (equal (url-type url-struct) "ws")
                   (make-network-process :name name
                                         :buffer buf-name
                                         :host (url-host url-struct)
                                         :service (url-port url-struct)
                                         :nowait nil)
                 (if (equal (url-type url-struct) "wss")
                     (error "Not implemented yet")
                   (error "Unknown protocol"))))
         (websocket (make-websocket :conn conn :url url :filter filter
                                    :close-callback close-callback
                                    :v75 websocket-use-v75)))
    (websocket-debug websocket "NEW websocket to %s" url)

    (lexical-let ((websocket websocket))
      ;; Calling `websocket-outer-filter' will "remove
      ;; connection strings" -- the HTTP-like headers --
      ;; and "only pass packets" -- but when we get a
      ;; chance to do something else with the output using
      ;; our own filter?
      (set-process-filter conn
                          (lambda (process output)
                            (websocket-outer-filter websocket output)))
      (when close-callback
        (set-process-sentinel conn
                              (lambda (process change)
                                (websocket-debug websocket
                                                 "State change to %s" change)
                                (unless (websocket-openp websocket)
                                  (funcall (websocket-close-callback
                                            websocket)))))))

    ;; for whatever reason the requests documented here
    ;; don't seem to be working, or even registering on
    ;; the server now.
    
    (websocket-debug websocket
                         (format "GET %s HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 8\r\nHost: %s\r\nSec-WebSocket-Origin: %s\r\nSec-WebSocket-Key: %s\r\n"
                                 (let ((path (url-filename url-struct)))
                                   (if (> (length path) 0) path "/"))
                                 (concat (url-host (url-generic-parse-url url))
                                         ":" (int-to-string (url-port (url-generic-parse-url url))))
                                 (concat "http://" (url-host (url-generic-parse-url url))
                                         ":" (int-to-string (url-port (url-generic-parse-url url))))
                                 key))

    (process-send-string conn
                         (format "GET %s HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 8\r\nHost: %s\r\nSec-WebSocket-Origin: %s\r\nSec-WebSocket-Key: %s\r\n"
                                 (let ((path (url-filename url-struct)))
                                   (if (> (length path) 0) path "/"))
                                 (concat (url-host (url-generic-parse-url url))
                                         ":" (int-to-string (url-port (url-generic-parse-url url))))
                                 (concat "http://" (url-host (url-generic-parse-url url))
                                         ":" (int-to-string (url-port (url-generic-parse-url url))))
                                 key))

    (websocket-debug websocket (format "To begin with, %s has status: %s"
                                       (websocket-conn websocket)
                                       (process-status (websocket-conn websocket))))
    websocket))

(defun websocket-debug (websocket msg &rest args)
  "In the WEBSOCKET's debug buffer, send MSG, with format ARGS."
  (when websocket-debug
    (let ((buf (get-buffer-create wave-debug-buffer)))
;;; old buffer
;; (format " *websocket %s debug*" (websocket-url websocket))
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (apply 'format (append (list msg) args))
          (insert (apply 'format (append (list msg) args)))
          (insert "\n"))))))

;;; Might need some debugging...?
(defun websocket-outer-filter (websocket output)
  "Removes connection strings, only passes packets."
;  (websocket-debug websocket "Received inflight: %s" output)
  (let ((start-point 0)
        (end-point 0)
        (text (concat (websocket-inflight-packet websocket) output)))
    (setq start-point (string-match "\0" text))
      (while (and start-point
                  (setq end-point
                        (string-match "\377" text start-point)))
        (funcall (websocket-filter websocket)
                 (substring text (+ 1 start-point) end-point))
        (setq start-point (string-match "\0" text end-point)))
      (let* ((next-start (or start-point
                                     (when end-point
                                       (or (string-match "\0" text end-point)
                                           (- (length text) 1)))
                                     0))
             (next-end (or (string-match "\377" text next-start)
                            (length text))))
        (setf (websocket-inflight-packet websocket)
              (concat (substring text next-start next-end))))))

(defun websocket-send (websocket text)
  "Send the raw TEXT as a websocket packet."
;;; Not going to ensure the thing is open for the moment,
;;; because that has been opening multiple copies of the
;;; connection.
;  (websocket-ensure-connected websocket)
  (unless (websocket-openp websocket)
    (error "No webserver process to send data to!"))
  (wave-debug "Sending on connection %s" (websocket-conn websocket))
  (wave-debug "   %s" text)
  (process-send-string (websocket-conn websocket)
                       (concat (unibyte-string ?\0) text
                               (unibyte-string ?\377))))

(defun websocket-openp (websocket)
  "Returns true if the websocket exists and is open."
  (wave-debug (format "...now %s has status: %s"
                      (websocket-conn linepad-connection)
                      (process-status (websocket-conn linepad-connection))))
  (and websocket (eq 'open (process-status (websocket-conn websocket)))))

;; This works weirdly, because it uses
;; `process-send-string' to (allegedly) kill the
;; connection before killing the buffer, but then the user
;; is still prompted as to whether or not the buffer
;; should be closed... and it's not clear that the process
;; really DOES stop when it gets the given unibyte string.
;; Maybe there's a better way to be sure to kill it.
(defun websocket-close (websocket)
  "Close the websocket and erase all the old websocket data."
  (websocket-debug websocket "Closing websocket")
  (when (websocket-openp websocket)
    (process-send-string (websocket-conn websocket) (unibyte-string ?\377?\0))
    (delete-process (websocket-conn websocket)))
  (kill-buffer (process-buffer (websocket-conn websocket))))

(defun websocket-ensure-connected (websocket)
  "If the websocket connection is closed, open it."
  (unless (and (websocket-conn websocket)
               (ecase (process-status (websocket-conn websocket))
                 ((run open listen) t)
                 ((stop exit signal closed connect failed nil) nil)))
    (websocket-close websocket)
    (let ((websocket-use-v75 (websocket-v75 websocket)))
      (websocket-open (websocket-url websocket)
                      (websocket-filter websocket)
                      (websocket-close-callback websocket)))))

(provide 'websocket)

;;; websocket.el ends here
