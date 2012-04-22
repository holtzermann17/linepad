;; changeset.el -- Reimplement the good parts of Changeset.js in Emacs Lisp

;;; Documentation:

;; This file reimplements many of the functions from
;; Etherpad's Changeset.js, providing (most of) an Emacs
;; client for Etherpad.  See also linepad.el (included in
;; this distribution).

;;; Comments:

;; M-x occur RET (defun\|(defclass\|(defalias RET 
;; is a good way to see what functions and classes this file defines.
;;
;; M-x occur RET = function ( RET
;; will show the functions defined in Changeset.js --
;;
;; clearly there are more things defined there.  However, not to worry!...
;;
;; If running into problems:
;;              * Check consistent use of "", nil, and 0 
;;              * Check that the code actually evaluates!
;;
;; Still to do:
;;
;;              * Check for TODO items in this file, e.g. like those in `linepad-changeset-to-elisp'
;;   
;;              * Fix the regexps here to make them work in Elisp!!
;;
;;              * What's the "entry point" for this code?  For now, it seems to be `applyToText'
;;                (so, particularly take a look at comments there b/c the implementation's not yet perfected)
;;
;;              * porting whatever else I need that has been defined in the Java world (??)
;; 
;;              * Run the tests
;;
;;              * check comments and docstrings
;;
;;              * connect to the connection-managing code in linepad.el (so we can have a buffer that
;;                shows changes coming across from Etherpad).
;;
;;              * implement pretty colors and stuff in Emacs (using old Arxana code)
;;
;;              * get the opposite direction going (sending messages encoded as changesets)
;;
;;              * have fun coding with people

(eval-when-compile
  (require 'cl))

(require 'radix)
(require 'json)
(require 'eieio)

(defclass attribute ()
        ((key :initarg :key
              :type string)
         (value :initarg :value
                :type string))
       "My Baseclass.")

(defmethod toString ((this attribute))
  (concat (slot-value this 'key) "," (slot-value this 'value)))

(defun string-to-attribute (str)
  (let ((initial (split-string str ",")))
    (if (eq (length initial) 2)
        (make-instance attribute :key (first initial) :value (second initial))
      (make-instance attribute :key "" :value ""))))  

(defclass attribute-pool-factory ()
  ((numToAttrib :initarg  :numToAttrib
                :type     list ;; alist
                :initform nil
                :documentation "e.g. {0 : [object attribute \"attribute\" \"foo\" \"bar\"]}")

   (attribToNum  :initarg  :AttribToNum
                 :type     list ;; alist
                 :initform nil
                 :documentation "e.g. {'foo,bar' : 0}")

   (nextNum  :initarg  :nextNum
             :type     number ;; alist
             :initform -1
             :documentation "A number"))
  "A pool of attributes (duh), and functions for operating on the same.")

;; Implementing everything using alists for now -- will switch to hashes later.

(defmethod putAttrib ((this attribute-pool-factory) attrib dontAddIfAbsent)
  (let* ((str (toString attrib))
         (lookup (assoc str (slot-value this 'attribToNum))))
    (cond 
     (lookup
        (cdr lookup))
     (dontAddIfAbsent
      -1)
     (t
      (let ((num  (1+ (slot-value this 'nextNum))))
        (set-slot-value this 'nextNum num)
        (set-slot-value this 'attribToNum (cons (cons str num) (assq-delete-all str (slot-value this 'attribToNum))))
        (set-slot-value this 'numToAttrib (cons (cons num (make-instance attribute 
                                                                         :key (if (slot-value attrib 'key)
                                                                                  (slot-value attrib 'key)
                                                                                "")
                                                                         :value (if (slot-value attrib 'value)
                                                                                    (slot-value attrib 'value)
                                                                                  "")))
                                                (assq-delete-all num (slot-value this 'numToAttrib))))
        num)))))

(defmethod getAttrib ((this attribute-pool-factory) num)
  (let ((pair (assoc num (slot-value this 'numToAttrib))))
      (toString (cdr pair))))

(defmethod getAttribKey ((this attribute-pool-factory) num)
  (let ((pair (assoc num (slot-value this 'numToAttrib))))
    (if (not pair)
         ""
      (slot-value (cdr pair) 'key))))

(defmethod getAttribValue ((this attribute-pool-factory) num)
  (let ((pair (assoc num (slot-value this 'numToAttrib))))
    (if (not pair)
        ""
      (slot-value (cdr pair) 'value))))

(defmethod eachAttrib ((this attribute-pool-factory) func)
  (mapcar (slot-value this 'numToAttrib)
          func))

(defmethod toJsonable ((this attribute-pool-factory))
  (return (json-encode `((numToAttrib . ,(slot-value this 'numToAttrib))
                         (nextNum . ,(slot-value this 'nextNum))))))

(defmethod fromJsonable ((this attribute-pool-factory) obj)
  (set-slot-value this 'numToAttrib (slot-value obj 'numToAttrib))
  (set-slot-value this 'nextNum (slot-value obj 'nextNum))
  (set-slot-value this 'attribToNum nil)
  (dolist (elt (slot-value this 'numToAttrib))
    (set-slot-value this 'attribToNum (cons 
                                       (cons (toString (cdr (assoc elt (slot-value this 'numToAttrib))))
                                             (car elt))
                                       (assq-delete-all num (slot-value this 'numToAttrib)))))
  this)

;; Actually not a true unit test, I should write one in a better style
(defun attribute-pool-factory-test ()
  (setq my-instance (make-instance attribute-pool-factory))
  (unless (eq (putAttrib my-instance (make-instance attribute :key "author" :value "a1") nil) 0)
    (message "error!"))
  (unless (eq (putAttrib my-instance (make-instance attribute :key "author" :value "a2") nil) 1)
    (message "error!"))
  (unless (eq (putAttrib my-instance (make-instance attribute :key "author" :value "a1") nil) 0)
    (message "error!"))
   (string= (getAttrib my-instance 0) "author,a1")
   (string= (getAttrib my-instance 1) "author,a2")
   (string= (getAttribKey my-instance 0) "author")
   (string= (getAttribKey my-instance 1) "author")
   (string= (getAttribValue my-instance 0) "a1")
   (string= (getAttribValue my-instance 0) "a2"))

;; generic code for working with base 36
(defvar linepad-base-36-lookup 
  (progn (setq count -1)
         (map 'list
              (lambda (str)
                (setq count (1+ count))
                (list str count))
              (mapcar 'char-to-string 
                      (append "0123456789abcdefghiklmnopqrstuvwxyz" nil)))))

(defun base-36-string-to-number (str)
  (let* ((input (reverse (mapcar 'char-to-string (append str nil))))
         (sum (second (assoc (car input) linepad-base-36-lookup)))
         (input (cdr input))
         (base 1))
    (while input
      (setq sum (+ sum (* (second (assoc (car input) linepad-base-36-lookup))
                          36)))
      (setq base (1+ base))
      (setq input (cdr input)))
    sum))

(defalias 'parseNum 'base-36-string-to-number)

(defun numToString (num)
  ;; `number-to-number' is the dependency on radix package, maybe replace with something local
  (downcase (number-to-number num 10 36)))
;; end of generic code for working with base 36

;; my homecooked functions for unpacking changesets begin here
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

(defun linepad-unpack-changeset (changeset)
  (mapcar (lambda (elt)
            (if (listp elt)
                (list (first elt) (base-36-string-to-number (second elt)))
              elt))
          (linepad-split-changeset changeset)))

; (linepad-unpack-changeset "Z:1v>5|2=17=m*0+5$auoeu")
; => ((":" 66) (">" 5) ("|" 2) ("=" 43) ("=" 21) ("*" 0) ("+" 5) "auoeu")

;;; Well, odds are that the above elisp version of the
;;; unpacked changeset isn't quite what the other
;;; functions are looking for
(defalias 'unpack 'linepad-unpack-changeset)

;; Probably need to test packing and unpacking to make
;; sure they are actually inverses?
(defun pack (oldLen, newLen, opsStr, bank)
  (let* ((lenDiff (- newLen oldLen))
         (lenDiffStr (if (>= lenDiff 0)
                         (concat ">" (numToString lenDiff))
                       (concat "<" (numToString (- lenDiff))))))
    (concat "Z:" (numToString oldLen) lenDiffStr opsStr "$" bank)))

;; NB. maybe I should follow the `unpack' routines from
;; EPLite more closely! - can double check and fix.  
(defun linepad-changeset-to-elisp (changeset)
"Produces a routine for insertion or deletion based on the specified CHANGESET.
The generated routine will either take a form like:
   (progn (goto-char CHAR) (insert STRING))
 or else
   (progn (goto-char CHAR) (delete-char N))"
;;  which could be simplified to (delete-region CHAR (+ N CHAR))
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
                     ;; `linepad-changeset-to-elisp' TODO: probably need to add a `next-line' instruction, actually?
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
                     ;; `linepad-changeset-to-elisp' TODO: again, this probably isn't what is needed here, see above.
                     (setq del (+ (second elt) prev-no))
                   (setq del (second elt)))
                 (setq routine (cons `(delete-char ,del) routine))
                 (setq prev-op "-"
                         prev-no (second elt))))
              ;; Apply the given attribute to the next `add' or `keep' operation
              ;; (we can build up a series of attributes to apply)...
              ;; `linepad-changeset-to-elisp' TODO: ignoring for now, but compare to `composeAttributes'
              ((equal (car elt) "*")
               (setq prev-op "*"
                     prev-no (second elt)))
              (t nil)))
          (nbutlast instructions 1))
    (reverse routine)))
;; homecooked functions for unpacking changesets end here

;; Pretty much the basic class here
;; in case it draws attention, note that (string-to-char "") => 0
(defclass bufOp ()
  ((opcode :initarg :opcode
           :type character
           :initform 0)
   (chars :initarg :chars
           :type number
           :initform 0)
   (lines :initarg :lines
           :type number
           :initform 0)
   (attribs :initarg :attribs
           :type list
           :initform nil)))

;; the `attribs' slot is different here: a string instead of a list
;; (double check...)
(defclass Operation ()
  ((opcode :initarg :opcode
           :type character
           :initform 0)
   (chars :initarg :chars
           :type number
           :initform 0)
   (lines :initarg :lines
           :type number
           :initform 0)
   (lastIndex :initarg :lastIndex
           :type number
           :initform 0)
   (attribs :initarg :attribs
           :type string
           :initform "")))

;; TODO this is an example of a class where Java has an init method, probably good to port
;; over here
(defclass opIterator ()
  ((curIndex :initarg :curIndex
           :type number
           :initform 0)
   (prevIndex :initarg :prevIndex
           :type number
           :initform 0)
   (ops :initarg :ops
           :type list
           :initform nil)
   ;; this needs to be adjusted to work with elisp
   (opRegex :initarg :opRegex
            :type string
            :initform "((?:\\*[0-9a-z]+)*)(?:\\|([0-9a-z]+))?([-+=])([0-9a-z]+)|\\?|")))

;; Maybe translates to something like this:
;;;;;;;; (defvar opIterator-regex "\\(\\*[0-9a-z]+\\)?*\\(\\|[0-9a-z]+\\)?\\([-+=]\\)\\([0-9a-z]+\\)\\?|")

(defmethod hasNext ((this opIterator))
  (< (slot-value this curIndex) (length (slot-value this 'ops))))

(defmethod nextRegexMatch ((this opIterator))
  (set-slot-value this 'prevIndex (slot-value this 'curIndex))
  (let* ((sq (subseq (slot-value this 'ops)
                     curIndex
                     (length (slot-value this 'ops))))
         (found (string-match (slot-value this 'opRegex) sq))
         result)
    (when found
      (setq result (make-instance Operation :opcode (match-string 3)
                                            :lines (parseNum (match-string 2))
                                            :chars (parseNum (match-string 4))
                                            :attribs (parseNum (match-string 4))))
      (set-slot-value this 'curIndex (+ (slot-value this 'curIndex)
                                        (slot-value result 'lastIndex))))
    (when result
      (when (eq (slot-value result 'opcode) (string-to-char "?"))
        (msg "Hit error opcode in op stream")))
    result))

(defmethod next ((this opIterator))
  (nextRegexMatch this))

(defclass Changeset ()
  ((oldLen :initarg :oldLen
           :type number
           :initform 0)
   (newLen :initarg :newLen
           :type number
           :initform 0)
   (ops :initarg :ops
           :type string
           :initform "")
   (charBank :initarg :charBank
           :type string
           :initform "")))

(defmethod toString ((this Changeset))
  (concat "[" oldLen "->" newLen "](" ops ")(" charBank ")"))

;; TODO Definitely need to have a close look at the `unpack' function and see what kind of
;; object it is supposed to return, since mine isn't returning an object at all!
;;
;; TODO We will presumably also need to implement `mutateTextLines', which is pretty similar.
;;
(defmethod applyToText ((this Changeset) str)
  "Applies a Changeset to a string, STR."
  (unless (equal (length str) (slot-value this 'oldLen))
    (warn (format "mismatched apply: %s / %s" (length str) oldLen)))
  (let* ((unpacked (unpack this))
         ;; TODO fix this
         (csIter (make-instance opIterator :ops (slot-value unpacked 'XXX)))
         (bankIter (make-instance stringIterator :str (slot-value this 'charBank)))
         (strIter (make-instance stringIterator :str str))
         (assem (make-instance stringAssembler))
         end)
    (while (and (not end) (hasNext csIter))
      (let ((op (next csIter)))
        (cond ((eq opcode (string-to-char "+"))
               (stringAppend assem (take bankIter (slot-value op 'chars)))
               (setq end t))
              ((eq opcode (string-to-char "-"))
               (skip stringIter (slot-value op 'chars))
               (setq end t))
              ((eq opcode (string-to-char "="))
               (stringAppend assem (take strIter (slot-value op 'chars)))
               (setq end t)))))
    (stringAppend assem (take strIter (slot-value strIter 'remaining)))
    (toString assem)))

;; Not entirely clear to me that this should be a
;; method...  I think I'll make it a generic function
(defun makeAttribsString (opcode attribs pool)
  (if (and (not pool)
           (not attribs))
      ""
    (when (> (length attribs) 0)
      (setq attribs (sort attribs)))
    (let (result)
      (loop for i from 0 to (length attribs) do
            (let ((pair (nth i attribs)))
              (when (or (eq opcode (string-to-char "="))
                        (and (eq opcode (string-to-char "+"))
                             (slot-value pair 'value)))
                ;; Not sure whether `pool' should be of type `AttribPool' or 
                ;; of type `AttribPoolFactory' (or what the difference is)
                (setq result (cons (concat "*" (numToString (putAttrib pool pair)))
                                   result)))))
      (apply #'concat result))))

;; This might take care of the attribute stuff needed in `linepad-changeset-to-elisp'??
(defun makeAttribsString (opcode attribs pool)
  (let (ret)
    (cond
     ((not attribs)
      (setq ret ""))
     ((stringp attribs)
      attribs)
     ((and pool attribs)
      (when (> (length attribs) 1)
        (setq attribs (sort attribs #'<)))
      (loop for i from 0 to (length attribs) do
            (let ((pair (nth i attribs)))
              (if (or (eq opcode (string-to-char "="))
                      (and (eq opcode (string-to-char "+"))
                           (slot-value pair 'value)))
                  (setq ret (cons (concat "*" (numToString (putAttrib pool pair))))))))
      ret))))


;; initial value of `assem' is supposed to be: `(or opStartIndex 0)' -- but maybe that can
;; be handled by code that calls this?  Similarly, `bufOpAdditionalCharsAfterNewline'
;; should be initialized to curIndex
(defclass mergingOpAssembler ()
        ((assem :initarg :assem
                :type number
                :initform 0)
         (bufOp :initarg :bufOp
                :type bufOp)
         (bufOpAdditionalCharsAfterNewline :initarg :bufOpAdditionalChars
                                           :type number))
       "Op Iterator class.")

(defmethod flush ((this mergingOpAssembler) &optional isEndDocument)
  (when (slot-value (slot-value this 'bufOp) 'opcode)
    (unless (and isEndDocument 
                 (eq (slot-value (slot-value this 'bufOp) 'opcode)
                     (string-to-char "="))
                 (not (slot-value (slot-value this 'bufOp) 'attribs)))
      (opAppend mergingOpAssembler (slot-value this 'bufOp))
      (when (slot-value this 'bufOpAdditionalCharsAfterNewline)
        (set-slot-value (slot-value this 'bufOp) 'chars (slot-value this 'bufOpAdditionalCharsAfterNewline))
        (set-slot-value (slot-value this 'bufOp) 'lines 0)
        (opAppend mergingOpAssembler (slot-value this 'bufOp))
        (set-slot-value this 'bufOpAdditionalCharsAfterNewline 0)))
    (set-slot-value (slot-value this 'bufOp) 'opcode 0)))

(defmethod opAppend ((this mergingOpAssembler) op)
  (when (> (slot-value op 'chars) 0)
    (if (and (eq (slot-value (slot-value this 'bufOp) 'opcode)
                 (slot-value op 'opcode))
             (eq (slot-value (slot-value this 'bufOp) 'attribs)
                 (slot-value op 'attribs)))
        (cond ((> (slot-value op 'lines) 0)
               (set-slot-value (slot-value this 'bufOp) 'chars (+ (slot-value (slot-value this 'bufOp) 'chars)
                                                                  (slot-value op 'chars)))
               (set-slot-value (slot-value this 'bufOp) 'lines (+ (slot-value (slot-value this 'bufOp) 'lines)
                                                                  (slot-value op 'lines)))
               (set-slot-value this 'bufOpAdditionalCharsAfterNewline 0))
              ((eq (slot-value op 'lines) 0)
               (set-slot-value (slot-value this 'bufOp) 'chars (+ (slot-value (slot-value this 'bufOp) 'chars)
                                                                  (slot-value op 'chars))))
              (t
               (set-slot-value this 'bufOpAdditionalCharsAfterNewline 
                               (+ (slot-value this 'bufOpAdditionalCharsAfterNewline)
                                  (slot-value op 'chars)))))
      (flush this)
      ;; needs to be implemented, check source to be sure
      (copyOp op bufOp))))

(defmethod endDocument ((this mergingOpAssembler))
  (flush this t))

(defmethod toString ((this mergingOpAssembler))
  (flush this)
  (toString (slot-value this 'assem)))

(defmethod clear ((this mergingOpAssembler))
  (clear (slot-value this 'assem))
  (clearOp bufOp))

(defclass stringAssembler ()
  ((pieces :initarg :pieces
           :type list
           :initform nil)))

(defmethod stringAppend ((this stringAssembler) str)
  (set-slot-value 'this pieces (cons str (slot-value 'this pieces))))

(defmethod toString ((this stringAssembler))
  (apply #'concat (slot-value 'this pieces)))

;; java version also has a `clear' method, but js doesn't
;; maybe java classes are always supposed to implement `clear'?
;; skipping for now.

(defclass smartOpAssembler ()
        ((minusAssem :initarg :minusAssem
                     :type mergingOpAssembler)
         (plusAssem :initarg :plusAssem
                     :type mergingOpAssembler)
         (keepAssem :initarg :keepAssem
                    :type mergingOpAssembler)
         (assem :initarg :assem
                :type stringAssembler)
         (lastOpcode :initarg :lastOpcode
                     :type character
                     :initform 0)
         (lengthChange :initarg :lengthChange
                       :type number
                       :initform 0)))

(defmethod flushKeeps ((this smartOpAssembler))
  (smartAppend (slot-value this 'assem)
               (toString (slot-value this 'keepAssem)))
  (clear (slot-value this 'keepAssem)))

;; TODO NB. This shows that we do need a clear method for `mergingOpAssembler'
(defmethod flushPlusMinus ((this smartOpAssembler))
  (smartAppend (slot-value this 'assem)
          (toString (slot-value this 'minusAssem)))
  (clear (slot-value this 'minusAssem))

  (smartAppend (slot-value this 'assem)
               (toString (slot-value this 'plusAssem)))
  (clear (slot-value this 'plusAssem)))

(defmethod smartAppend ((this smartOpAssembler) op)
  (unless (or (eq (slot-value op 'opcode) 0)
              (eq (slot-value op 'chars) 0))
    (cond ((eq (slot-value op 'opcode) (string-to-char "-"))
           (when (eq (slot-value this 'lastOpcode) (string-to-char "=")) (flushKeeps this))
           (opAppend (slot-value this 'minusAssem) op)
           (set-slot-value this 'lengthChange (- (slot-value this 'lengthChange)
                                                 ;; I think length is what's intended???
                                                 (length (slot-value op 'chars)))))
          ((eq (slot-value op 'opcode) (string-to-char "+"))
           (when (eq (slot-value this 'lastOpcode) (string-to-char "=")) (flushKeeps this))
           (opAppend (slot-value this 'plusAssem) op)
           (set-slot-value this 'lengthChange (- (slot-value this 'lengthChange)
                                                 ;; I think length is what's intended??
                                                 (length (slot-value op 'chars)))))
          ((eq (slot-value op 'opcode) (string-to-char "="))
           (when (not (eq (slot-value this 'lastOpcode) (string-to-char "="))) (flushPlusMinus this))
           (opAppend (slot-value this 'keepAssem) op)))
    (set-slot-value this 'lastOpcode (slot-value op 'opcode))))

;; Depends on the function `makeAttribsString' which is
;; implemented as a method in the js and java versions,
;; but just as a generic function here (above).
(defmethod appendOpWithText ((this smartOpAssembler) opcode text attribs pool)
  (let ((op (make-instance bufOp :opcode opcode :attribs (makeAttribsString opcode attribs pool)))
        (lastNewlinePos (with-temp-buffer (insert text)
                                          ; (goto-char (point-max))
                                          (if (re-search-backward "\n" nil t)
                                              (point)
                                            0))))
    (cond ((eq lastNewlinePos 0)
           (set-slot-value op 'chars (length text))
           (set-slot-value op 'lines (length 0))
           (smartAppend this op))
          (t
           (set-slot-value op 'chars (1+ lastNewlinePos))
           (set-slot-value op 'lines (length (split-string text "\n")))
           (smartAppend this op)
           (set-slot-value op 'chars (- (length text)
                                        (1+ lastNewlinePos)))
           (set-slot-value op 'lines 0)
           (smartAppend this op)))))

(defmethod toString ((this smartOpAssembler))
  (flushPlusMinus this)
  (flushKeeps this)
  (toString (slot-value 'this assem)))

(defmethod endDocument ((this smartOpAssembler)) 
  (endDocument (slot-value 'this keepAssem)))

(defmethod getLengthChange ((this smartOpAssembler)) 
  (slot-value 'this lengthChange))

(defclass stringIterator ()
  ((str :initarg :str
           :type string)
   (curIndex :initarg :curIndex
             :type number)))

(defmethod remaining ((this stringIterator))
  (- (length (slot-value this 'str))
     (slot-value this 'curIndex)))

(defmethod peek ((this stringIterator) n)
  (when (<= n (remaining this))
    (msg "Not enough remaining"))
  (substring curIndex n))

(defmethod skip ((this stringIterator) n)
  (when (<= n (remaining this))
    (msg "Not enough remaining"))
  (set-slot-value this 'curIndex (+ (slot-value this 'curIndex)
                                    n)))

(defmethod take ((this stringIterator) n)
  (when (<= n (remaining this))
    (msg "Not enough remaining"))
  (let* ((new (+ (slot-value this 'curIndex)
                 n))
         (s (substring (slot-value this 'str) curIndex new)))
    (set-slot-value this 'curIndex new)
    s))

;; This does not actually do the merge, it just computes a
;; string showing how the merge would be done
(defun followAttributes (att1 att2 pool)
  "Takes two lists of numbers ATT1 and ATT2, describing attributes, and a POOL of attributes.
The merge of two sets of attribute changes to the same
text takes the lexically-earlier value if there are two
values for the same key.  Otherwise, all key/value changes
from both attribute sets are taken.  This operation is the
`follow': a set of changes is produced that can be applied
to ATT1 to produce the merged set."
      (cond ((or (not att2) (not pool)) "")
            ((not att1) att2)
            (t (let ((atts (mapcar (lambda (a) (getAttrib pool (parseNum a)))
                                   att2)))
                 (mapc (lambda (a)
                         (let ((pair1 (getAttrib pool (parseNum a)))
                               (found nil)
                             (loop for i from 0 to (length atts) while (not found) do
                                   (when (eq (slot-value (nth i atts) 'key) 
                                             (slot-value (getAttrib pool (parseNum a)) 'key))
                                     (when (<= (slot-value (nth i atts) 'value) 
                                               (slot-value (getAttrib pool (parseNum a)) 'value))
                                       (delete-if (lambda (x) t) atts :start i :count 1))
                                     (setq found t)))
                       att1)
                 (with-temp-buffer (dolist (a atts)
                                     (insert "*")
                                     (insert (numToString (putAttrib pool (parse a)))))
                                   (buffer-substring-no-properties (point-min) (point-max))))))))))

;; used in `follow' - NB. this is a "closure" making use
;; of Emacs's scoping properties (it would be good to
;; check that that actually works) - so, variables not
;; defined here should be defined there.
;;
;; I don't really understand what this function does - it
;; might be good to try and understand...
(defun followOperationCombiner (op1 op2 opOut)
  (cond ((or (eq (slot-value op1 'opcode) (string-to-char "+"))
             (eq (slot-value op2 'opcode) (string-to-char "+")))
         (let ((whichToDo (cond ((not (eq (slot-value op1 'opcode) (string-to-char "+"))) 2)
                               ((not (eq (slot-value op2 'opcode) (string-to-char "+"))) 1)
                               (t
                                (let ((firstChar1 (string-to-char chars1))
                                      (firstChar2 (string-to-char chars2))
                                      (insertFirst1 (hasInsertFirst (slot-value op1 'attribs)))
                                      (insertFirst2 (hasInsertFirst (slot-value op2 'attribs))))
                                  (cond ((and insertFirst1 (not insertFirst2)) 1)
                                        ((and insertFirst2 (not insertFirst1)) 2)
                                        ((and (eq firstChar1 (string-to-char "\n"))
                                              (not (eq firstChar2 (string-to-char "\n"))))
                                         2)
                                        ((and (not (eq firstChar1 (string-to-char "\n")))
                                              (eq firstChar2 (string-to-char "\n")))
                                         1)
                                        (reverseInsertOrder 2)
                                        (t 1)))))))
           (if (eq whichToDo 1)
               (progn
                 (skip chars1 (slot-value op1 'chars))
                 (set-slot-value opOut 'opcode (string-to-char "="))
                 (set-slot-value opOut 'lines (slot-value op1 'lines))
                 (set-slot-value opOut 'chars (slot-value op1 'chars))
                 (set-slot-value opOut 'attribs 0)
                 (set-slot-value op1 'opcode 0))
             (skip chars2 (slot-value op2 'chars))
             (copyOp op2 opOut)
             (set-slot-value op2 'opcode 0))))
        ;; outer `cond' clause...
        ((eq (slot-value op1 'opcode) (string-to-char "-"))
         (if (not  (eq (slot-value op2 'opcode) 0))
             (set-slot-value op1 'opcode 0)
           (if (<= (slot-value op1 'chars (slot-value op2 'chars)))
               (progn 
                 (set-slot-value op2 'chars (- (slot-value op2 'chars)
                                               (slot-value op1 'chars)))
                 (set-slot-value op2 'lines (- (slot-value op2 'lines)
                                               (slot-value op1 'lines)))
                 (set-slot-value op1 'opcode 0)
                 (when (not (eq (slot-value op2 'chars) 0))
                   (set-slot-value op2 'opcode 0)))
             (set-slot-value op1 'chars (- (slot-value op1 'chars)
                                           (slot-value op2 'chars)))
             (set-slot-value op1 'lines (- (slot-value op1 'lines)
                                           (slot-value op2 'lines)))
             (set-slot-value op2 'opcode 0))))
        ;; outer `cond' clause
        ((eq (slot-value op2 'opcode) (string-to-char "-"))
         (copyOp op2 opOut)
         (cond ((not (eq (slot-value op1 'opcode) 0))
                (set-slot-value op2 'opcode 0))
               ((<= (slot-value op2 'chars) (slot-value op1 'chars))
                (set-slot-value op1 'chars (- (slot-value op1 'chars)
                                              (slot-value op2 'chars)))
                (set-slot-value op1 'lines (- (slot-value op1 'lines)
                                              (slot-value op2 'lines)))
                (set-slot-value op2 'opcode 0)
                (when (not (eq (slot-value opt1 'chars) 0))
                  (set-slot-value op1 'opcode 0)))
               (t
                (set-slot-value opOut 'chars (slot-value op1 'chars))
                (set-slot-value op2 'lines (- (slot-value op2 'lines)
                                              (slot-value op1 'lines)))
                (set-slot-value op2 'chars (- (slot-value op2 'chars)
                                              (slot-value op1 'chars)))
                (set-slot-value op1 'opcode 0))))
        ;; outer `cond'
        ((not (eq (slot-value op1 'opcode) 0))
         (copyOp op2 opOut)
         (set-slot-value op2 'opcode 0))
        ((not (eq (slot-value op2 'opcode) 0))
         (copyOp op1 opOut)
         (set-slot-value op1 'opcode 0))
        ;; end of outer `cond'
        (t
         (set-slot-value opOut 'opcode (string-to-char "="))
         (set-slot-value opOut 'attribs (followAttributes (slot-value op1 'attribs)
                                                          (slot-value op2 'attribs)
                                                          pool))
         (if (<= (slot-value op1 'chars) (slot-value op2 'chars))
             (progn
               (set-slot-value opOut 'chars  (slot-value op1 'chars))
               (set-slot-value opOut 'lines  (slot-value op1 'lines))
               (set-slot-value op2 'lines (- (slot-value op2 'lines)
                                             (slot-value op1 'lines)))
               (set-slot-value op2 'chars (- (slot-value op2 'chars)
                                             (slot-value op1 'chars)))
               (set-slot-value op1 'opcode 0)
               (when (not (eq (slot-value op2 'chars) 0))
                 (set-slot-value op2 'opcode 0)))
           (set-slot-value opOut 'chars  (slot-value op2 'chars))
           (set-slot-value opOut 'lines  (slot-value op2 'lines))
           (set-slot-value op1 'lines (- (slot-value op1 'lines)
                                         (slot-value op2 'lines)))
           (set-slot-value op1 'chars (- (slot-value op1 'chars)
                                         (slot-value op2 'chars)))
           (set-slot-value op2 'opcode 0))))
  ;; now set some values in the calling context
  (cond ((eq (slot-value opOut 'opcode) (string-to-char "="))
         (setq oldPos (+ oldPos (slot-value opOut chars)))
         (setq newLen (+ newLen (slot-value opOut chars))))
        ((eq (slot-value opOut 'opcode) (string-to-char "-"))
         (setq oldPos (+ oldPos (slot-value opOut chars))))
        ((eq (slot-value opOut 'opcode) (string-to-char "+"))
         (setq newLen (+ newLen (slot-value opOut chars))))))

;; The docstring here is fairly vague.
(defun slicerZipperFunction (attOp csOp opOut &optional pool)
  "Function used as parameter for `applyZip' to apply a Changeset to an attribute.
ATTOP is the op from the sequence that is being operated
on, either an attribution string or the earlier of two
exportss being composed.  POOL is optional."
  (cond ((eq (slot-value attOp 'opcode) (string-to-char "-"))
         (copyOp attOp opOut)
         (set-slot-value attOp 'opcode 0))
        ((not (eq (slot-value attOp 'opcode) 0))
         (copyOp csOp opOut)
         (set-slot-value csOp 'opcode 0))
        ((eq (slot-value csOp 'opcode) (string-to-char "-"))
         (if (<= (slot-value csOp 'chars) (slot-value attOp 'chars))
             (progn
               (when (eq (slot-value attOp 'opcode) (string-to-char "="))
                 (set-slot-value opOut 'opcode (string-to-char "-"))
                 (set-slot-value opOut 'chars (slot-value csOp 'chars))
                 (set-slot-value opOut 'lines (slot-value csOp 'lines))
                 (set-slot-value opOut 'attrib 0))
               (set-slot-value attOp 'chars (- (slot-value attOp chars)
                                               (slot-value csOp 'chars)))
               (set-slot-value attOp 'lines (- (slot-value attOp lines)
                                               (slot-value csOp 'lines)))
               (set-slot-value csOp 'opcode 0)
               (when (not (eq (slot-value attOp 'chars) 0))
                 (set-slot-value atOp 'opcode 0)))
           ;; ... otherwise do more or less the opposite
           (when (eq (slot-value attOp 'opcode) (string-to-char "="))
             (set-slot-value opOut 'opcode (string-to-char "-"))
             (set-slot-value opOut 'chars (slot-value attOp 'chars))
             (set-slot-value opOut 'lines (slot-value attOp 'lines))
             (set-slot-value opOut 'attrib 0))
           (set-slot-value csOp 'chars (- (slot-value csOp 'chars)
                                          (slot-value attOp chars)))
           (set-slot-value csOp 'lines (- (slot-value csOp 'lines)
                                          (slot-value attOp lines)))
           (set-slot-value attOp 'opcode 0)))

        ((eq (slot-value csOp 'opcode) (string-to-char "+"))
         (copyOp csOp opOut)
         (set-slot-value csOp 'opcode 0))
        ((eq (slot-value csOp 'opcode) (string-to-char "="))
         (if (<= (slot-value csOp 'chars) (slot-value attOp 'chars))
             (progn 
               (set-slot-value opOut 'opcode (slot-value attOp 'opcode))
               (set-slot-value opOut 'chars (slot-value csOp 'chars))
               (set-slot-value opOut 'lines (slot-value csOp 'lines))
               (set-slot-value opOut 'attrib (composeAttributes (slot-value attOp 'attribs)
                                                                (slot-value csOp 'attribs)
                                                                (slot-value attOp 'opcode)
                                                                pool))
               (set-slot-value csOp 'opcode 0)
               (set-slot-value attOp 'chars (- (slot-value attOp chars)
                                               (slot-value csOp 'chars)))
               (set-slot-value attOp 'lines (- (slot-value attOp lines)
                                               (slot-value csOp 'lines)))
               (when (not (eq (slot-value attOp 'chars) 0))
                 (set-slot-value atOp 'opcode 0)))
           (set-slot-value opOut 'opcode (slot-value attOp 'opcode))
           (set-slot-value opOut 'chars (slot-value attOp 'chars))
           (set-slot-value opOut 'lines (slot-value attOp 'lines))
           (set-slot-value opOut 'attrib (composeAttributes (slot-value attOp 'attribs)
                                                            (slot-value csOp 'attribs)
                                                            (slot-value attOp 'opcode)
                                                            pool))
           (set-slot-value attOp 'opcode 0)
           (set-slot-value csOp 'chars (- (slot-value csOp 'chars)
                                           (slot-value attOp chars)))
           (set-slot-value csOp 'lines (- (slot-value csOp 'lines)
                                           (slot-value attOp lines)))))
        ((eq (slot-value csOp 'opcode) (string-to-char 0))
         (copyOp attOp opOut)
         (set-slot-value attOp 'opcode 0))))

;; called from `applyToAttribution' (see below).  Note
;; the `func' argument supplied there is `slicerZipperFunction'
;;
;; It is also called from `follow' with a different `func';
;; argument.
(defun applyZip (in1 idx1 in2 idx2 func)
  (let ((iter1 (make-instance opIterator :opStr in1 :startIndex idx1))
        (iter2 (make-instance opIterator :opStr in2 :startIndex idx2))
        (assem (make-instance smartOpAssembler))
        (op1 (make-instance bufOp))
        (op2 (make-instance bufOp))
        (opOut (make-instance bufOp)))
    (while (or (not (eq (slot-value op1 'opcode) 0))
               (hasNext iter1)
               (not (eq (slot-value op2 'opcode) 0))
               (hasNext iter2))
      (when (and (eq (slot-value op1 'opcode) 0)
                 (hasNext iter1))
        (setq op1 (next iter1)))
      (when (and (eq (slot-value op2 'opcode) 0)
                 (hasNext iter2))
        (setq op2 (next iter2)))
      ;;; ??? not sure what to do here... Note that the js
      ;;; file is very different from the java file in
      ;;; this function.
      (func op1 op2 opOut)
      (when (not (eq (slot-value opOut 'opcode) 0))
        (append assem opOut)
        (set-slot-value opOut 'opcode 0)))
      (edDocument assem)
      (toString assem)))

;; Original documentation not so clear about what attr
;; argument is.  Is it a string "*3*4" or an attribute
;; object?  Since we run a `string-match' on it, we must
;; be assuming it's a string.
(defun attributeTester (attr pool)
  "Returns a function that tests if an array of attributes
  contains a given attribute ATTR that is already present
  in the pool."
  (if (not pool)
      (lambda (a) nil)
    (let ((attribNum (putAttrib pool attr t)))
      (if (< attribNum 0)
          (lambda (a) nil)
        (lambda (a) 
          (string-match (concat "\\*" (numToString attribNum) "(?!\\w)")
                        attr))))))

;; instead of using a `FollowOperationCombiner' object
;; like in Java, I will use a `followOperationCombiner'
;; function.  AFAICT is the only place (so far) where a
;; real "closure" is used.  Worth double checking that
;; things actually work the way I remember!
(defun follow (cs1 cs2 reverseInsertOrder pool)
  (let ((unpacked1 (unpack cs1))
        (unpacked2 (unpack cs2))
        (len1 (slot-value unpacked1 'oldLen))
        (len2 (slot-value unpacked2 'oldLen)))
    (unless (eq len1 len2)
      (warn "mismatched follow"))
    ;; Maybe I do need a `stringIterator' class then,
    ;; since a `stringIterator' has a `curIndex' and
    ;; that's important.
    (let ((chars1 (make-instance stringIterator (slot-value unpacked1 'charBank)))
          (chars2 (make-instance stringIterator (slot-value unpacked2 'charBank)))
          (oldLen (slot-value unpacked1 'newLen))
          ;; `oldPos' and `newLen' will be set in the "closure"
          (oldPos 0) 
          (newLen 0) 
          ;; `hasInsertFirst' is a function that will be run in the "closure"
          (hasInsertFirst (attributeTester (make-instance attribute :key "insertorder" :value "first") pool))
          (newOps (applyZip (slot-value unpacked1 'ops)
                            (slot-value unpacked2 'ops)
                            (slot-value unpacked2 'ops)
                            0
                            #'followOperationCombiner)))
      (pack oldLen
            (+ newLen (- oldLen oldPos))
            newOps
            (charBank unpacked2)))))

;; The doc string's description of the strings and their
;; meaning leaves something to be desired.  Someone
;; (probably me!) should explain the relationship between
;; strings of this form, actual lists of attributes.
;;
;; Furthermore the java code indicates that composing
;; `att2' with a blank `att1' ALWAYS yeilds `att2',
;; i.e. not just in the case of mutation.  That is also
;; indicade by the examples, but seems potentially weird,
;; and it's hard to tell without more effort whether the
;; javascript behaves that way or not.
(defun composeAttributes (att1 att2 resultIsMutation pool)
  "ATT1 and ATT2 are strings like \"*3*f*1c\", and RESULTISMUTATION is a boolean.
If RESULTISMUTATION is false, attributes are 
treated as attribute presence information; otherwise, they
are treated as operations that mutate another set of
attributes.

Examples, of the form (att1Items, att2Items, resultIsMutation) => result :

   ([], [(bold, )], true)              => [(bold, )]
   ([], [(bold, )], false)             => []
   ([], [(bold, true)], true)          => [(bold, true)]
   ([], [(bold, true)], false)         => [(bold, true)]
   ([(bold, true)], [(bold, )], true)  => [(bold, )]
   ([(bold, true)], [(bold, )], false) => []

In particular, in the case of a mutation, an att2 composed
with an empy att1 is just att2.

POOL can be null if ATT2 has no attributes."
  (cond ((and (eq (length att1) 0) resultIsMutation) 
         att2)
        ((eq (length att2) 0)
         att1)
        ((not pool) "")
        (t
         (let (atts (mapcar (lambda (a) 
                              (getAttrib pool (parseNum a)))
                            att1))
           (mapc (lambda (a)
                   (let ((pair (getAttrib pool (parseNum a)))
                         (found nil))
                     (loop for i from 0 to (length atts) while (not found) do
                           (when (eq (slot-value (nth i atts) 'key)
                                     (slot-value (getAttrib pool (parseNum a)) 'key))
                             (if (or (slot-value (nth i atts) 'value) 
                                     resultIsMutation)
                                 (set-slot-value (nth i atts) 'value (slot-value pair 'value))
                               (delete-if (lambda (x) t) atts :start i :count 1))
                             (setq found t)))
                     (when (and (not found)
                                (or (slot-value pair 'value)
                                    resultIsMutation))
                       (cons pair atts))))
                 att2)
           (sort atts)
           (with-temp-buffer (dolist (a atts)
                               (insert "*")
                               (insert (numToString (putAttrib pool (parse a)))))
                             (buffer-substring-no-properties (point-min) (point-max)))))))

(defun applyToAttribution (cs str pool)
  (applyZip str 0 (slot-value cs 'ops) #'slicerZipperFunction))

(provide 'changeset)
