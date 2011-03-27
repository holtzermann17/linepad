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

            ;; TODO: it would be good to get this set as a
            ;; proper changeset, and to unpack it in the
            ;; manner that is done below.  I believe I
            ;; understand changesets well enough to know
            ;; how to do this now.
            (setq changesets (cons (concat "Z:0>"
                                           (plist-get init-text :attribs)
                                           "$"
                                           (plist-get init-text :text))
                                   changesets))))

        ;; ONGOING PHASE: keep track of any changesets
        ;; that are coming from the server.  Actually
        ;; keeping track of the "unpacked" versions at
        ;; present (see below).
        (when (and data (plist-get data :changeset))
          (setq changesets (cons (linepad-unpack-changeset 
                                  (plist-get data :changeset))
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

;; I'm not actually sure that this string represents the
;; digits of base 36, partly because there aren't 36
;; things here.  I guess "10" is 36 in base 36...  but
;; then you'd suppose "z" would be 35 and that's not what
;; we get here.

(defvar linepad-base-36-lookup 
  (progn (setq count -1)
         (map 'list
              (lambda (str)
                (setq count (1+ count))
                (list str count))
              (mapcar 'char-to-string 
                      (append "0123456789abcdefghiklmnopqrstuvwxyz" nil)))))

(defun base-36-string-to-number (str)
;;  v = v * b + digit;
  (let* ((input (reverse (mapcar 'char-to-string (append str nil))))
         (sum (second (assoc (car input) linepad-base-36-lookup)))
         (input (cdr input))
         (base 1))
    (while input
      (setq sum (+ sum
                   (* (second (assoc (car input) linepad-base-36-lookup))
                      36)))
      (setq base (1+ base))
      (setq input (cdr input)))
    sum))

(base-36-string-to-number "1z")

; (regexp-opt '("*" "+" "=" "|" ">" ":" "$") t)
;; Based on `split-string' from subr.el
(defun linepad-split-changeset (string)
  (let ((rexp "\\([$*+:=>|]\\)")
	(start 0)
	notfirst
        s
	(list nil))
    (while (and (string-match 
                 rexp string
                 (if (and notfirst
                          (= start 
                             (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (setq s (match-string-no-properties 1))
      (if (< start (match-beginning 0))
	  (setq list
		(cons (cons (substring string
                                        (max (- start 1)
                                             0)
                                        start)
                            (list 
                             (substring string 
                                 start
                                 (match-beginning 0))
                             ))
		      list)))
      (setq start (match-end 0)))
    (if (< start (length string))
	(setq list
	      (cons (substring string start) list)))
    ;; get rid of the Z
    (cdr (nreverse list))))

;; this code is presumably inefficient, should be done
;; with nconc or whatever
(defun linepad-unpack-changeset (changeset)
  (mapcar (lambda (elt)
            (if (listp elt)
                (list (first elt) (base-36-string-to-number (second elt)))
              elt))
          (linepad-split-changeset changeset)))

; (linepad-unpack-changeset "Z:1v>5|2=17=m*0+5$auoeu")
; => ((":" 66) (">" 5) ("|" 2) ("=" 43) ("=" 21) ("*" 0) ("+" 5) "auoeu")

;(linepad-changeset-to-elisp "Z:1v>5|2=17=m*0+5$auoeu")

;; The generated routine will either take a form something like:
;;   (progn (goto-char CHAR) (insert STRING))
;; or else
;;   (progn (goto-char CHAR) (delete-char N))
;; which can be simplified to
;;   (delete-region CHAR (+ N CHAR))

(defun linepad-changeset-to-elisp (changeset)
  (let* ((instructions (linepad-unpack-changeset changeset))
         (bank (car (last instructions)))
         routine
         prev-op
         prev-no)
    (mapc (lambda (elt)
            ;; trying this just to be able to use case with its `eql' comparison
            (cond 
              ;; purely for the sake of information, can
              ;; presumably ignore
              ((equal (car elt) ":") (setq prev-op ":"
                         prev-no (second elt)))
              ;; we know we will not delete characters
              ;; (if associated number is positive, add that many)
              ((equal (car elt) ">")
               (setq prev-op ">"
                     prev-no (second elt)))
              ;; we know we will delete this many characters
              ((equal (car elt) "<") (setq prev-op ">"
                         prev-no (second elt)))
              ;; we know we will go ahead a certain number
              ;; of lines but we don't know why yet
              ((equal (car elt) "|") (setq prev-op "|"
                         prev-no (second elt)))
              ;; skip over N characters
              ;; including |L newlines if previous operator was |
              ((equal (car elt) "=")
               (let (fwd)
                 (if (equal prev-op "|")
                     ;; TODO: probably need to add a
                     ;; `next-line' instruction, actually
                     (setq fwd (+ prev-no (second elt)))
                   (setq fwd (second elt)))
                 (setq routine (cons `(forward-char ,fwd) routine))
               (setq prev-op "="
                     prev-no (second elt))))
              ;; add N characters from bank
              ;; including |L newlines if previous operator was |
              ((equal (car elt) "+")
               (let (add)
                 (if (equal prev-op "|")
                     (setq add (concat (let ((blanks ""))
                                         (dotimes (i prev-no)
                                           (concat blanks "\n"))
                                         blanks)
                                       (substring bank 0 (second elt))))
                   (setq add (substring bank 0 (second elt))))
                 (setq routine (cons `(insert ,add) routine))
                 (setq prev-op "+"
                       prev-no (second elt))))
              ;; delete N characters
              ;; including |L newlines if previous operator was |
              ((equal (car elt) "-")
               (let (del)
                 (if (equal prev-op "|")
                     ;; TODO: again, this probably isn't
                     ;; what is needed here
                     (setq del (+ (second elt) prev-no))
                   (setq del (second elt)))
                 (setq routine (cons `(delete-char ,del) routine))
                 (setq prev-op "-"
                         prev-no (second elt))))
              ;; apply the give attribute to the next add
              ;; or keep operation (we can build up a
              ;; series of attributes to apply)... ignore for now
              ((equal (car elt) "*") (setq prev-op "*"
                         prev-no (second elt)))
              (t nil)))
          (nbutlast instructions 1))
    (reverse routine)))

;; it would be nice if changes that were inserted via
;; linepad were not undoable by the local user.  I'm
;; guessing that Jan has things set up that way in Rudel.
;; Can check.
(defun linepad-apply-changeset (changeset)
  (save-excursion (set-buffer (get-buffer-create "*Linepad*"))
                  (goto-char (point-min))
                  (eval `(progn ,@(linepad-changeset-to-elisp changeset)))))

; (linepad-apply-changeset "Z:1v>5|2=17=m*0+5$auoeu")

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; end of section for managing changesets
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;; linepad.el ends here
