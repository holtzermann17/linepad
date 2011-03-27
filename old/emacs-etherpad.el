;;; emacs-etherpad.el -- demo a connection of Emacs and Etherpad

;; Copyright (C) 2010, 2011 Joseph Corneli <holtzermann17@gmail.com>
;; Copyright (C) 2010 Jan Moringen <scymtym@gmx.net>

;; Time-stamp: <2011-02-13 23:44:27 joe>

;; Tags: real-time demo

;;; Commentary:

;; Running M-x eval-buffer will open a channel to Etherpad.
;; Changesets are currently received in the buffer
;; " *http metameso.org:9000*<10>"  (no quotes, yes space).
;; You can submit a demo post to Etherpad by running
;; M-x try-post-to-etherpad.  You can also post to the chat
;; by running M-x try-post-to-etherpad-chat.

;; At present, the code only submits one changeset, but
;; this serves as a proof of concept. 

;;; Todo:

;; This file represents about 10 days of hacking, and
;; needs to be cleaned up to work with the Rudel
;; framework.  Code describing this framework is at
;; https://code.launchpad.net/~scymtym/rudel/etherpad

;; And Emacs needs to learn how to speak the EasySync
;; protocol fluently!!

;; Jan and Joe made a few "convenience" changes to Emacs
;; dealing with issues around listing buffers related to
;; this process; these would be worth finding and
;; packaging up if possible.

;;; Thanks:

;; Thanks to Peter Martischka for help in getting
;; reasonable information out of Etherpad!!

;;; Code:

;; in order to keep track of what request
;; corresponds to what response, we use a counter.

(require 'json)
(require 'url-vars)
(require 'url-http)
(require 'cl)

(defvar url-request-data nil)
(defvar url-request-extra-headers nil)
(defvar default-user-vars nil)

(setq url-mime-charset-string
      "ISO-8859-1,utf-8;q=0.7,*;q=0.7")

(defvar user-handle "JamesHacker1")

(defvar host "metameso.org")   ; where the service is
(defvar port 9000)             ; port number
(defvar pad-name "name")       ; name of the pad
(setq pad-name "random")       ; set it to something novel for "clean slate" -- should be a `read-string' or similar
(defvar id-val "3")            ; this will be set to a random id for the *session*
(defvar key-val "9")           ; this will be set to another separate identifier for the session
(defvar finished nil)          ; keep track of state
(defvar counter 1)             ; keep track of number of attempts, used by `rudel-etherpad-handle-http-request'
(defvar depth 0)               ; similarly

(defvar baserev 0)             ; relevant for managing changesets and chat requests
(setq baserev 0)               ; reset to 0 every time the buffer is evaluated
(defvar posting-seq 1)         ; as above (and presumably redundant with the above)
(setq posting-seq 1)           ; as above

(defvar cache-pad-html nil)    ; will store the existing content of the pad; used by `rudel-etherpad-pad-exists-p'

;;;
;; FUNCTIONS FOR LOGGING
;;;

(defun rudel-log-etherpad-message (a)
  (with-current-buffer (get-buffer-create "*etherpad-log*")
    (save-excursion (goto-char (point-max))
                    (insert (format "%s\n" a)))))

(defun rudel-log-etherpad-response (a &optional c d)
  (let ((content (buffer-string)))
    (set (make-local-variable 'rudel-etherpad-last-request) url-request-data)
    ;; here we can detect a message that says "restart-fail"
    (with-current-buffer (get-buffer-create "*etherpad-log*")
      (save-excursion
        (goto-char (point-max))
        (insert
         (format "%d(%d)=> STATUS %s\n%d(%d)=> CONTENT\n%s\n"
                 c d a c d content))))
    (setq finished content)))

(defun rudel-log-etherpad-response-minimal (a &optional c d)
  (with-current-buffer (get-buffer-create "*etherpad-log*")
    (save-excursion (goto-char (point-max))
                    (insert (format "%d(%d)=> %s\n" c d a))))
  (setq finished (buffer-string)))

(defun clear-rudel-log-etherpad-messages ()
  (with-current-buffer (get-buffer-create "*etherpad-log*")
    (save-excursion (erase-buffer))))

;;;
;; FUNCTIONS FOR SPEAKING IN URLS
;;;

(defun rudel-etherpad-make-m-value (pad-name username color-id initial-client-vars)
  ;; we may have to do some more work to have the real
  ;; external ip address in a portable way.
  (let ((ip-address (substring
		     (mapconcat
		      #'number-to-string
		      (first (network-interface-info
			      (car (first (network-interface-list)))))
		      ".") 0 -2)))
    (rudel-etherpad-url-encode-string
     ;; `json-encode' allows us to set up "m" messages properly
     (json-encode
      `((type . "COLLABROOM")
	(data
	 (type     . "CLIENT_READY")
	 (roomType . "padpage")
	 (roomName . ,(format "padpage/%s" pad-name))
	 (data
	  ;; we try using the information we have!
	  (lastRev . ,(plist-get initial-client-vars :revision))
	  (stats
           ;; full useragent string
	   (useragent . ,(substring (url-http-user-agent-string) 0 -2))
           ;; Etherpad Server tells us our IP address so
           ;; we can use that here
	   (ip        . ,(plist-get initial-client-vars :client-ip))
	   (screen    . "1664,891,1848,1060,1848,1155"))
	  (userInfo
	   (userAgent . "URL/Emacs")
	   (colorId   . ,color-id)
	   (ip        . ,(plist-get initial-client-vars :client-ip))
	   (name      . ,username)
	   ;; should obtain from server or cookie?
	   (userId    . ,(plist-get initial-client-vars :user-id))))))))))

;; basically just copied from w3m.el
(defun rudel-etherpad-url-encode-string (str)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((and (char-equal ch ?\x20))
	      "%20")
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string into a list of chars.
          str)))

;; jac - note reversed sense of last optional argument
(defun rudel-etherpad-handle-http-request
  (url-template &optional method minimalp repeat-p)
  ;; repeat-p turned off by default
  (let* ((r-value     (number-to-string (random 1e12)))
	 (url         (format url-template r-value))
	 (url-request-method (case method
                               (post
                                "POST")
                               (t
                                "GET"))))
    (rudel-log-etherpad-message
     (concat (format "%d(%d)=> " counter depth)
             url-request-method " " url))
    ;; we will now send a request and so indicate we will
    ;; be waiting for a response; the response handlers
    ;; will tell us if/when we stop waiting
    (setq finished nil)
    (url-retrieve url (if minimalp
			  #'rudel-log-etherpad-response-minimal
			#'rudel-log-etherpad-response)
                  (list counter depth))

    (let ((remaining 100))
      (while (and (not finished) (not (zerop remaining)))
	(decf remaining)
	(sit-for 0.1))))

  ;; if we wanted to repeat more than once, we would have
  ;; to explicitly say that here
  (when repeat-p
    (unless finished
      ;; just introducing an ad-hoc way to exit the recursion --
      (unless (> depth 4)
	(let ((depth (1+ depth)))
	  (rudel-log-etherpad-message "repeating request")
	  (rudel-etherpad-handle-http-request
	   url-template method minimalp))))

    ;; we can detect restart-fail, but soon we will need to
    ;; figure out just what to do in case of restart-fail.
    ;; Here we just repeat the request, which may not really
    ;; work out to our advantage.
    (when (string-match-p "restart-fail" finished)
      (unless (> depth 4)
	(let ((depth (1+ depth)))
	  (rudel-log-etherpad-message "repeating request")
	  (rudel-etherpad-handle-http-request
	   url-template method minimalp)))))

  (when (zerop depth)
    (incf counter)))

;;;
;; FUNCTIONS FOR MANAGING EXISTING PADS AND CREATING NEW PADS
;;;

(defun rudel-etherpad-pad-exists-p (host port pad-name)
  "Check whether a pad PAD-NAME exists on server HOST on PORT."
  (let ((status 'no-result-yet)
	(url-max-redirections 1))
    ;; Try to retrieve the pad. Store the response status which
    ;; indicates success or failure.
    (url-retrieve (format "http://%s:%d/%s"
			  host port pad-name)
		  (lambda (status1)
		    (setq status status1
			  cache-pad-html (buffer-string))))
    ;; wait until the callback has stored the status
    (while (eq status 'no-result-yet)
      (sit-for 0.1))
    (cond
     ;; A status of nil means the page could be retrieved directly
     ;; without redirect. This indicates that the pad exists.
     ((null status)
      (message "%s exists!" pad-name)
      t)

     ;; A redirect to the pad creation URL indicates pad PAD-NAME does
     ;; not yet exist.
     ((equal status
             (list
              :redirect
              (format "http://%s:%d/ep/pad/create?padId=%s"
                      host port pad-name)))
      nil)

     (t (error "unexpected response")))))

(defun rudel-etherpad-create-pad (host port pad-name)
  "Create pad PAD-NAME on server HOST on PORT."
  ;; Pad name has to be in content and referer (sic) URL.
  (let ((url-request-extra-headers
	 (list
	  ;; anything we want to use over and over again
	  ;; we could move into `rudel-etherpad-handle-http-request',
	  ;; for example, r values could be generated there.
	  (cons "Connection" "keep-alive")
	  (cons "Keep-Alive" "115")
	  (cons "Content-Type" "application/x-www-form-urlencoded")
	  (cons "Referer" (format "http://%s:%d/ep/pad/create?padId=%s"
				  host port pad-name))))
	(url-request-data (format "padId=%s" pad-name)))

    (rudel-etherpad-handle-http-request
     (format "http://%s:%d/ep/pad/create"
	     host port) 'post t)))

(defun rudel-etherpad-ensure-pad (host port pad-name)
  "Create PAD-NAME on server HOST on PORT, if it doesn't exist yet."
  (unless (rudel-etherpad-pad-exists-p host port pad-name)
    (sit-for 0.1)
    (rudel-etherpad-create-pad host port pad-name)
    ;; to renew the cache
    (rudel-etherpad-pad-exists-p host port pad-name)))

;; - Worth knowing the difference between "text" and "initial revisions"
;; - Maybe we can get the name of the pad out of the initial JSON?
(defun rudel-etherpad-analyze-pad-json (json-string)
  (let* ((initial-data      (let ((json-array-type 'list))
			      (json-read-from-string json-string)))
	 (user-id           (cdr (assoc 'userId initial-data)))
	 (client-ip         (cdr (assoc 'clientIp initial-data)))
	 (client-vars       (cdr (assoc 'collab_client_vars initial-data)))
	 (revision          (cdr (assoc 'rev client-vars)))
	 (attributed-text   (cdr (assoc 'initialAttributedText client-vars)))
	 (attributes        (cdr (assoc 'attribs attributed-text)))
	 (text              (cdr (assoc 'text attributed-text)))
	 (initial-revisions (cdr (assoc 'initialRevisionList initial-data))))
    (message " user-id           %s" user-id)
    (message " client-ip         %s" client-ip)
    (message " revision          %s" revision)
    (message " attributes        %s" attributes)
    (message " text              %s" text)
    (message " initial-revisions %s" initial-revisions)
    (list :initial-revisions initial-revisions
          :text              text
          :attributes        attributes
          :revision          revision
          :client-ip         client-ip
          :user-id           user-id)))

;;;
;; FUNCTIONS FOR USING THE CHANNEL WE SET UP IN emacs-etherpad-channel.el
;;;

;; not sure what nextNum represents
(defun rudel-etherpad-make-changeset-request
  (revision changeset user-id)
  ;; starting with json-read-from-string on an "m"
  ;; that we pulled out of firebug
  (rudel-etherpad-url-encode-string
   (json-encode
    `((type . "COLLABROOM")
      (data
       (type      . "USER_CHANGES")
       (baseRev   . ,revision)
       (changeset . ,changeset)
       (apool (nextNum . 1)
              (numToAttrib
               (\0 . ("author" ,user-id)))))))))

(defun rudel-etherpad-make-chat-request (user-id text)
  (rudel-etherpad-url-encode-string
   (json-encode
    `((type . "COLLABROOM")
      (data
       (type . "CLIENT_MESSAGE")
       (payload
	(type       . "chat")
	(senderName . ,user-handle)
	(authId     . ,user-id)
	(lineText   . ,text)
	(userId     . ,user-id)))))))

;;;
;; POSTING A CHANGESET
;;;

;; TODO use rudel-etherpad-make-m-value
(defun try-post-to-etherpad ()
  (interactive)
  (let ((url-request-data
	 (concat
	  "m="
	  (rudel-etherpad-make-changeset-request
	   baserev
           "Z:54>e*0+e$hello etherpad"
	   (plist-get default-user-vars :user-id))))
	(url-request-extra-headers
	 (list (cons "Content-Type" "application/x-www-form-urlencoded"))))

    ;; NOTE: it is important to use the correct `id-val'
    ;; -- it is currently stored in a global variable
    ;; after it is set
    (rudel-etherpad-handle-http-request
     ;; we think seq is 1 here because this is the first content to be posted...
     (format
      "http://%s:%d/comet/post?v=2&r=%%s&id=%s&seq=%d"
      host port id-val posting-seq)
     'post))
  ;; The first change we submit will result in an
  ;; additional "anonymous" change that adds a newline to
  ;; the end of the pad, so after submitting the first
  ;; change, we increase baserev from 0 to 2, and seq from
  ;; 1 to 3.  Every subsequent change we increase these
  ;; numbers by 1 only.
  (case baserev
    (0
     (setq baserev (+ 2 baserev))
     (setq posting-seq (+ 2 posting-seq)))
    (t
     (setq baserev (+ 1 baserev))
     (setq posting-seq (+ 1 posting-seq))))) 

;;;
;; MAKING A CHAT POST
;;;

(defun try-post-to-etherpad-chat ()
  (interactive)
  (let ((url-request-data
	 (concat
	  "m="
	  (rudel-etherpad-make-chat-request
           ;; Note that it is possible to put any string
           ;; here, but we use the `user-id' by convention :)
	   (plist-get default-user-vars :user-id)
	   "this works great")))
	(url-request-extra-headers
	 (list (cons "Content-Type" "application/x-www-form-urlencoded"))))
    (rudel-etherpad-handle-http-request
     ;; Maybe seq is 0 here?
     (format "http://%s:%d/comet/post?v=2&r=%%s&id=%s&seq=%d"
	     host port id-val posting-seq)
     'post)))

;;;
;; FUNCTIONS FOR MANAGING THE EMACS ENVIRONMENT
;;;

;; This is based on the function by the same name from
;; files.el, I added the "noprompt" flag - jac
(defun kill-matching-buffers (regexp &optional internal-too noprompt)
  "Kill buffers whose name matches the specified REGEXP.
The optional second argument indicates whether to kill
internal buffers too.  The optional third argument
specifies whether or not to prompt when killing a modified
buffer."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (if noprompt
            (kill-buffer buffer)
          (kill-buffer-ask buffer))))))

(defun kill-etherpad-client ()
  "Simple function for deleting left-over buffers."
  (interactive)
  (let (kill-buffer-hook)
    (kill-matching-buffers ".*http.*" t t)))

;;; emacs-etherpad.el ends here

;;;
;; DEFINITIONS FOR REGRESSION TESTING BEGIN HERE
;;;
(when (require 'ert nil t)

  (ert-deftest rudel-etherpad-test-pad-exists-pad ()
    "Test `rudel-etherpad-pad-exists-p'."
    (let ((host "metameso.org")
          (port 9000))
      (should-not (rudel-etherpad-pad-exists-p
                   host port "does-not-exist"))
      (should (rudel-etherpad-pad-exists-p
               host port "pepsi-pad"))))

  (ert-deftest rudel-etherpad-test-pad-creation ()
    "Test `rudel-etherpad-create-pad'."
    (let ((host     "metameso.org")
          (port     9000)
          (pad-name (format "%d" (random))))
      (should-not (rudel-etherpad-pad-exists-p
                   host port pad-name))

      (rudel-etherpad-create-pad
       host port pad-name)

      (should (rudel-etherpad-pad-exists-p
               host port pad-name))))

  (ert-deftest rudel-ehterpad-test-analyze-pad-json ()
    "Test `rudel-etherpad-analyze-pad-json'."
    (should
     (equal
      (rudel-etherpad-analyze-pad-json
       "{\"userAgent\":\"URL/Emacs (i686-pc-linux-gnu; X11)\",\"accountPrivs\":{\"maxRevisions\":100},\"initialRevisionList\":[],\"initialOptions\":{\"guestPolicy\":\"deny\"},\"collab_client_vars\":{\"initialAttributedText\":{\"text\":\"\\nWelcome to EtherPad!\\n\\nThis pad text is synchronized as you type, so that everyone viewing this page sees the same text.  This allows you to collaborate seamlessly on documents!\\n \\n\",\"attribs\":\"|5+50\"},\"clientIp\":\"92.39.27.128\",\"clientAgent\":\"URL/Emacs (i686-pc-linux-gnu; X11)\",\"padId\":\"does-it-still-work\",\"historicalAuthorData\":{},\"apool\":{\"numToAttrib\":{},\"nextNum\":0},\"rev\":0,\"globalPadId\":\"does-it-still-work\"},\"colorPalette\":[\"#ffc7c7\",\"#fff1c7\",\"#e3ffc7\",\"#c7ffd5\",\"#c7ffff\",\"#c7d5ff\",\"#e3c7ff\",\"#ffc7f1\",\"#ff8f8f\",\"#ffe38f\",\"#c7ff8f\",\"#8fffab\",\"#8fffff\",\"#8fabff\",\"#c78fff\",\"#ff8fe3\",\"#d97979\",\"#d9c179\",\"#a9d979\",\"#79d991\",\"#79d9d9\",\"#7991d9\",\"#a979d9\",\"#d979c1\",\"#d9a9a9\",\"#d9cda9\",\"#c1d9a9\",\"#a9d9b5\",\"#a9d9d9\",\"#a9b5d9\",\"#c1a9d9\",\"#d9a9cd\"],\"clientIp\":\"92.39.27.128\",\"userIsGuest\":true,\"userColor\":7,\"padId\":\"does-it-still-work\",\"initialTitle\":\"Public Pad\",\"opts\":{},\"chatHistory\":{\"start\":0,\"historicalAuthorData\":{},\"end\":0,\"lines\":[]},\"numConnectedUsers\":1,\"isProPad\":false,\"serverTimestamp\":1280582723371,\"globalPadId\":\"does-it-still-work\",\"userId\":\"g.0pdn8hkepmyd3w3q\",\"cookiePrefsToSet\":{\"fullWidth\":false,\"hideSidebar\":false},\"hooks\":{\"aceCreateDomLine\":[{\"hook\":\"aceCreateDomLine\",\"plugin\":\"heading1\",\"type\":\"client\"},{\"hook\":\"aceCreateDomLine\",\"plugin\":\"twitterStyleTags\",\"type\":\"client\"},{\"hook\":\"aceCreateDomLine\",\"plugin\":\"wikiStyleLinks\",\"type\":\"client\"}],\"aceInitInnerdocbodyHead\":[{\"hook\":\"aceInitInnerdocbodyHead\",\"plugin\":\"twitterStyleTags\",\"type\":\"client\"},{\"hook\":\"aceInitInnerdocbodyHead\",\"plugin\":\"wikiStyleLinks\",\"type\":\"client\"}],\"collectContentPre\":[{\"hook\":\"collectContentPre\",\"plugin\":\"heading1\",\"type\":\"client\"}],\"aceAttribsToClasses\":[{\"hook\":\"aceAttribsToClasses\",\"plugin\":\"heading1\",\"type\":\"client\"}],\"aceGetFilterStack\":[{\"hook\":\"aceGetFilterStack\",\"plugin\":\"twitterStyleTags\",\"type\":\"client\"},{\"hook\":\"aceGetFilterStack\",\"plugin\":\"wikiStyleLinks\",\"type\":\"client\"}],\"collectContentPost\":[{\"hook\":\"collectContentPost\",\"plugin\":\"heading1\",\"type\":\"client\"}]}}")
      (list :initial-revisions '()
            :text              "\nWelcome to EtherPad!\n\nThis pad text is synchronized as you type, so that everyone viewing this page sees the same text.  This allows you to collaborate seamlessly on documents!\n \n"
            :attributes        "|5+50"
            :revision          0
            :client-ip         "92.39.27.128"
            :user-id           "g.0pdn8hkepmyd3w3q")))
    )

  ;; (string=
  ;;
  ;;  "m=%7B%22type%22%3A%22COLLABROOM%22%2C%22data%22%3A%7B%22type%22%3A%22USER_CHANGES%22%2C%22baseRev%22%3A92%2C%22changeset%22%3A%22Z%3Adp%3E4%3D8*0%2B4%24aoeu%22%2C%22apool%22%3A%7B%22numToAttrib%22%3A%7B%220%22%3A%5B%22author%22%2C%22g.f1cdjkg0wgsjrsr0%22%5D%7D%2C%22nextNum%22%3A1%7D%7D%7D"

  )

;;; Code graveyard

;;;
;; ;; STEP 8 : POST REQUEST -- SET UP ANOTHER STREAMING CHANNEL
;; ;; body: oob=useChannel%3A2%3Astreaming
;; ;;;

;;       (let ((url-request-data "oob=useChannel%3A2%3Astreaming")
;; 	    (url-request-extra-headers
;; 	     (list (cons "Content-Type" "application/x-www-form-urlencoded"))))
;;         (rudel-etherpad-handle-http-request
;;          (format "http://%s:%d/comet/post?v=2&r=%%s&id=%s&seq=0"
;;                  host port id-val)
;; 	 'post))
;;                                         ;(sleep-for 1.0)

;; ;;;
;; ;; STEP 9 : GET REQUEST -- CONFIRM RECEIPT OF STREAMING CHANNEL?
;; ;;;

;;       (let ((url-request-extra-headers
;;              (list
;;               (cons "Connection" "keep-alive")
;;               (cons "Keep-Alive" "115")
;;               (cons "Content-Type" "application/x-www-form-urlencoded")
;;               (cons "Referer" (format
;;                                "http://%s:%d/%s"
;;                                host port pad-name)))))
;;         (rudel-etherpad-handle-http-request
;;          (format "http://%s:%d/comet/channel?v=2&r=%%s&id=%s&channel=streaming&new=yes&create=yes&seq=0"
;;         	 host port id-val)
;;          'get))

;;;
;; STEP ? : SET UP XHR (formerly known as "mystery step")
;;;

;; (let ((url-request-extra-headers
;;        (list
;;         (cons "Connection" "keep-alive")
;;         (cons "Keep-Alive" "115")
;;         (cons "Referer" (format
;;                          "http://%s:%d/comet/"
;;                          host port)))))
;;   (rudel-etherpad-handle-http-request
;;    (format "http://%s.%s:%d/comet/xhrXdFrame"
;;            key-val host port)
;;    'get))


