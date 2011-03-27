;;; linepad.el -- demo a connection of Emacs and Etherpad Lite

;; Copyright (C) 2010, 2011 Joseph Corneli <holtzermann17@gmail.com>
;; Copyright (C) 2010 Jan Moringen <scymtym@gmx.net>

;; Time-stamp: <2011-02-13 23:44:27 joe>

;; Tags: real-time demo

;;; Commentary:

;; Can currently connect to Etherpad Lite, but can't do
;; anything else yet.

;;; Code:

(require 'json)
(require 'websocket)

;; currently used as a source of `wave-client-ws-filter',
;; which we will want to replace with our own filter
;; function.
(require 'wave-client-websocket) 

(defvar linepad-connection)
(defvar linepad-beat 1)
(defvar linepad-pad-name "test")

(defvar baserev 0)
(defvar user-id "unknown")
(defvar changesets nil)

(defun linepad-five-seconds-hence ()
  (substring (current-time-string
            (let ((now (current-time)))
              (list (first now)
                    (+ (second now) 5)))) 11 19))

(defun linepad-heartbeat ()
  (websocket-send linepad-connection
                  (let* ((message (format "~h~%s" linepad-beat))
                         (len (length message)))
                    (format "~m~%s~m~%s" len message)))
  (setq linepad-beat (1+ linepad-beat)))

(defun linepad-token ()
  (let ((avail (mapcar 'char-to-string 
                       (append "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz" nil)))
        (result ""))
    (dotimes (i 20)
      (setq result (concat (nth (random 61) avail)
                           result)))
    (concat "t." result)))

(defun linepad-form-initial-request ()
  (let* ((json-part
          (replace-regexp-in-string 
           " "
           ""
           (json-encode
            `((type  . "CLIENT_READY")
              (padId .  ,linepad-pad-name)
              (token . ,(linepad-token))
              (protocolVersion . 1)))))
         (len (length json-part)))
    (concat (format "~m~%s~m~~j~" (+ 3 len))
            json-part)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Importing things from wave-client-websocket.el and modifying them here
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun linepad-filter (packet)
  "Filter OUTPUT from our websocket.  As websocket responses come
back, parse them and call the appropriate callbacks."
  ;; TODO(ohler): redo this properly
  (let ((json-object-type 'plist)
        (json-key-type nil))
    (wave-debug "Received packet: %s" packet)
    ;; Perhaps we should just trust that packets are
    ;; complete -- compare the wave client filter for a
    ;; way to deal with unfinished packets
    (when (string-match "}$" packet)
      (let* ((header-end (string-match "{" packet))
             (header (substring packet 0 header-end))
             (json-part (substring packet header-end))
             (response (json-read-from-string
                        (wave-client-ws-fixup-control-codes
                         json-part)))
             (data (plist-get response :data)))

        ;; STARTUP PHASE 
        (when (equal user-id "unknown")
          (setq user-id (plist-get response :userId))
          (let* ((client-vars (plist-get response :collab_client_vars))
                 (init-text (plist-get client-vars :initialAttributedText)))
            (setq baserev (plist-get client-vars :rev))
            (setq changesets (cons (concat "Z:0>"
                                           (plist-get init-text :attribs)
                                           "$"
                                           (plist-get init-text :text))
                                   changesets))))

        ;; ONGOING PHASE: keep track of any changesets
        ;; that are coming from the server.
        (when (and data (plist-get data :changeset))
          (setq changesets (cons (plist-get data :changeset)
                                 changesets))
          (setq baserev (plist-get data :newRev)))

        ;; See wave-client-websocket.el for more ideas
        ;; about what to do with content here.
        (wave-debug "response: %s" (pp-to-string response))
        (wave-debug "user-id: %s" user-id)
        (wave-debug "baserev: %s" baserev)
       ))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; end of import from wave-client-websocket.el
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun linepad-initiate-connection ()
  (setq linepad-beat 0)
  (setq baserev 0)
  (setq user-id "unknown")
  (setq changesets nil)
  (setq linepad-connection  
        (websocket-open
         "ws://localhost:9001/socket.io/websocket"
         'linepad-filter))
  (sleep-for 1.0)
  (websocket-send linepad-connection
                  (linepad-form-initial-request))
  (run-at-time (linepad-five-seconds-hence) 5
               #'linepad-heartbeat))

; (revision changeset user-id)
;; baserev
;; "Z:54>e*0+e$hello etherpad"
;; (plist-get default-user-vars :user-id)

(setq changeset "Z:t>d*0+d$helloetherpad")

;; Note: in messages TO the server, say baseRev
;; whereas messages from the server say newRev
(defun linepad-send-text (changeset)
  ;; long term we don't want to replace spaces in the
  ;; whole string!
  (let* ((json-part (replace-regexp-in-string 
                     " " ""
                     (json-encode
                      `((type . "COLLABROOM")
                        (data
                         (type      . "USER_CHANGES")
                         (baseRev   . ,baserev)
                         (changeset . ,changeset)
                         (apool (numToAttrib
                                 (\0 . ("author" ,user-id)))
                                (nextNum . 1)))))))
         (len (length json-part)))
    (websocket-send linepad-connection 
                    (concat (format "~m~%s~m~~j~" 
                                    (+ 3 len))
                            json-part)))
  (setq baserev (1+ baserev)))

(defun linepad-terminate-connection ()
  (websocket-close linepad-connection))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; section for managing changesets
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun linepad-unpack-changeset (changeset))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; end of section for managing changesets
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;;; linepad.el ends here
