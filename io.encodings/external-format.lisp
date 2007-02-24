;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :io.encodings)

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

;; Mostly taken from SBCL's sb-simple-streams contrib
;;; **********************************************************************
;;; This code was written by Paul Foley
;;;

;;; Sbcl port by Rudi Schlatte.

(define-condition octet-encoding-error (error)
  ((string :initarg :string :reader octets-encoding-error-string)
   (position :initarg :position :reader octets-encoding-error-position)
   (external-format :initarg :external-format
                    :reader octets-encoding-error-external-format))
  (:report (lambda (c s)
             (format s "Unable to encode character ~A as ~S."
                     (char-code (char (octets-encoding-error-string c)
                                      (octets-encoding-error-position c)))
                     (octets-encoding-error-external-format c)))))

(define-condition illegal-character (octet-encoding-error) ())


(define-condition octet-decoding-error (error)
  ((array :initarg :array :accessor octet-decoding-error-array)
   (start :initarg :start :accessor octet-decoding-error-start)
   (end :initarg :end :accessor octet-decoding-error-end)
   (position :initarg :position :accessor octet-decoding-bad-byte-position)
   (external-format :initarg :external-format
                    :accessor octet-decoding-error-external-format))
  (:report
   (lambda (c s)
     (format s "Illegal ~A character starting at byte position ~D: ~A."
             (octet-decoding-error-external-format c)
             (octet-decoding-bad-byte-position c)
             (cffi:mem-aref (octet-decoding-error-array c) :uint8
                            (octet-decoding-bad-byte-position c))))))

(define-condition end-of-input-in-character (octet-decoding-error) ())
(define-condition malformed-multibyte-sequence (octet-decoding-error) ())
(define-condition invalid-starter-octet (malformed-multibyte-sequence) ())
(define-condition invalid-continuation-octet (malformed-multibyte-sequence) ())
(define-condition overlong-octet-sequence (malformed-multibyte-sequence) ())
(define-condition illegal-code-point (octet-decoding-error) ())

;;;
;;;
;;; EXTERNAL-FORMAT
;;;
;;;

(deftype line-terminator ()
  '(member :unix :mac :dos))

(defvar *default-external-format* :utf-8)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-line-terminator* :unix))

(defvar *external-formats* (make-hash-table))
(defvar *external-format-aliases* (make-hash-table))
(defvar *external-format-list* nil)

(defstruct (external-format
             (:conc-name ef-)
             (:print-function %print-external-format)
             (:constructor %make-external-format (name
                                                  line-terminator
                                                  octet-size
                                                  octets-to-char
                                                  char-to-octets)))
  (name (missing-arg) :type keyword :read-only t)
  (line-terminator (missing-arg) :type keyword)
  (octets-to-char (missing-arg) :type function :read-only t)
  (char-to-octets (missing-arg) :type function :read-only t)
  (octet-size (missing-arg) :type real))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity nil)
    (format stream "~A ~S"
            (ef-name ef) (ef-line-terminator ef))))

(defun make-external-format (name &key new-name
                             (line-terminator *default-line-terminator*)
                             (octet-size 1.5))
  (check-type line-terminator line-terminator)
  (let ((ef (find-external-format name)))
    (%make-external-format
     (or new-name (ef-name ef))
     (or line-terminator (ef-line-terminator ef))
     (if (and octet-size (<= 1 octet-size 4))
         octet-size
         (ef-octet-size ef))
     (ef-octets-to-char ef)
     (ef-char-to-octets ef))))

;;;
;;; UTILS
;;;
(deftype octet ()
  '(unsigned-byte 8))

(deftype buffer-index ()
  'fixnum)

(defmacro add-external-format (name aliases ef)
  (let (($alias$ (gensym "ALIAS")))
    `(progn
       (setf (gethash ,name *external-formats*) ,ef)
       (setf *external-format-list* (append *external-format-list* (list ,name)))
       (dolist (,$alias$ ',aliases)
         (assert (keywordp ,$alias$))
         (setf (gethash ,$alias$ *external-format-aliases*) ,name)))))

(defmacro define-external-format (name aliases octet-size octets-to-char char-to-octets
                                  &key (line-terminator *default-line-terminator*))
  (let (($ef$ (gensym "EF")))
    `(macrolet ((to-char (&body body)
                  `(lambda (input output error-fn bytes-left)
                     (declare (type (function () octet) input)
                              (type (function (character) t) output)
                              (type (function (symbol) character) error-fn)
                              (type buffer-index bytes-left)
                              (ignorable input output error-fn bytes-left))
                     ,@body))
                (to-octets (&body body)
                  `(lambda (input output error-fn chars-left)
                     (declare (type (function () character) input)
                              (type (function (octet) t) output)
                              (type (function (symbol) character) error-fn)
                              (type buffer-index chars-left)
                              (ignorable input output error-fn chars-left))
                     ,@body)))
       (let ((,$ef$ (%make-external-format ,name ,line-terminator ,octet-size
                                           ,octets-to-char ,char-to-octets)))
         (add-external-format ,name ,aliases ,$ef$)))))

(defun find-external-format (name &optional (error-p t))
  (when (external-format-p name)
    (return-from find-external-format name))

  (when (eq name :default)
    (setq name *default-external-format*))
  (when (stringp name)
    (setf name (iolib-utils:ensure-keyword name)))

  (or (gethash name *external-formats*)
      (gethash (gethash name *external-format-aliases*)
               *external-formats*)
      (if error-p (error "External format ~S not found." name) nil)))

;;;
;;;
;;; EXTERNAL FORMATS
;;;
;;;

(define-condition void-external-format (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Attempting I/O through void external-format."))))

(define-external-format :void () 0
  (to-char
   (error 'void-external-format))
  (to-octets
   (error 'void-external-format)))

(define-external-format :ascii (:us-ascii) 1
  (to-char
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (let ((code (funcall input)))
     (if (< code 128)
         (funcall output (aref +iso-8859-1-table+ code))
         (funcall output (funcall error-fn 'illegal-code-point)))))
  (to-octets
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (let ((code (char-code (funcall input))))
     (if (< code 128)
         (funcall output code)
         (funcall output (char-code (funcall error-fn 'illegal-character)))))))

(define-external-format :iso-8859-1 (:iso8859-1 :ISO_8859-1 :latin1 :l1
                                     :csISOLatin1 :iso-ir-100 :CP819) 1
  (to-char
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (let ((code (funcall input)))
     (funcall output (aref +iso-8859-1-table+ code))))
  (to-octets
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (let ((code (char-code (funcall input))))
     (if (< code 256)
         (funcall output code)
         (funcall output (char-code (funcall error-fn 'illegal-character)))))))

(defmacro define-iso-8859-external-formats (indexes)
  (flet ((get-name-and-aliases (index)
           (if (endp index)
               (values index nil)
               (values (car index)
                       (cdr index)))))
    `(progn
       ,@(loop :for i :in indexes
            :collect
            (multiple-value-bind (index aliases) (get-name-and-aliases i)
              (let ((table (iolib-utils:concat-symbol "+iso-8859-" index "-table+"))
                    (name (iolib-utils:ensure-keyword
                           (concatenate 'string "ISO-8859-" index))))
                (push (iolib-utils:ensure-keyword
                       (concatenate 'string "ISO8859-" index))
                      aliases)
                `(define-external-format ,name ,aliases 1
                   (to-char
                    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
                    (let ((code (funcall input)))
                      (funcall output (aref ,table code))))
                   (to-octets
                    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
                    (let ((code (position (funcall input) ,table)))
                      (if code
                          (funcall output code)
                          (funcall output
                                   (position (funcall error-fn 'illegal-character)
                                             ,table))))))))))))

(define-iso-8859-external-formats
    (("2" :ISO_8859-2 :latin2 :l2 :csISOLatin2 :iso-ir-101)
     ("3" :ISO_8859-3 :latin3 :l3 :csISOLatin3 :iso-ir-109)
     ("4" :ISO_8859-4 :latin4 :l4 :csISOLatin4 :iso-ir-110)
     ("5" :ISO_8859-5 :cyrillic :csISOLatinCyrillic :iso-ir-144)
     ("6" :ISO_8859-6 :arabic :csISOLatinArabic :iso-ir-127)
     ("7" :ISO_8859-7 :greek :greek8 :csISOLatinGreek :iso-ir-126)
     ("8" :ISO_8859-8 :hebrew :csISOLatinHebrew :iso-ir-138)
     ("9" :ISO_8859-9 :latin5 :l5 :csISOLatin5 :iso-ir-148)
     ("10" :ISO_8859-10 :latin6 :l6 :csISOLatin6 :iso-ir-157)
     ("11" :ISO_8859-11 :thai :csISOLatinThai :iso-ir-166)
     ("13" :ISO_8859-13 :baltic :csISOLatinBaltic :iso-ir-179)
     ("14" :ISO_8859-14 :iso-celtic :latin8 :l8 :csISOLatinCeltic :iso-ir-199)
     ("15" :ISO_8859-15 :latin9 :l9 :csISOLatin9 :iso-ir-203)
     ("16" :ISO_8859-16 :latin10 :l10 :csISOLatin10 :iso-ir-226)))

(iolib-utils:define-constant +max-unicode-code-point+ #x10FFFF)

(declaim (inline illegal-unicode-code-point))
(defun illegal-unicode-code-point (code)
  (declare (type (unsigned-byte 32) code))
  (or (<= #xD800 code #xDFFF)
      (= code #xFFFE)
      (= code #xFFFF)
      (> code +max-unicode-code-point+)))

(define-external-format :utf-8 (:utf8) 2
  (to-char
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (block utf-8-decode
     (let ((code 0) (bytes-needed nil)
           (byte0 0) (byte1 0)
           (byte2 0) (byte3 0))
       (declare (type octet byte0 byte1 byte2 byte3))
       (labels ((decode-err (sym)
                  (return-from utf-8-decode
                    (funcall output (funcall error-fn sym))))
                (utf-8-byte-len (code)
                  (declare (type octet code))
                  (cond
                    ((not (logbitp 7 code))                  1)
                    ((= (logand code #b11100000) #b11000000) 2)
                    ((= (logand code #b11110000) #b11100000) 3)
                    ((= (logand code #b11111000) #b11110000) 4)
                    (t (decode-err 'invalid-starter-octet))))
                (valid-secondary-check (byte)
                  (or (= (logand byte #b11000000) #b10000000)
                      (decode-err 'invalid-continuation-octet)))
                (overlong-check (starter mask)
                  (or (/= starter byte0)
                      (/= (logior byte1 mask) mask)
                      (decode-err 'overlong-octet-sequence))))
         (macrolet ((put-and-check-valid-secondary-bytes (&rest places)
                      `(progn ,@(reduce #'append places
                                        :key #'(lambda (x) `((setf ,x (funcall input))
                                                             (valid-secondary-check ,x)))))))
           (setf byte0 (funcall input)
                 bytes-needed (utf-8-byte-len byte0))
           (when (< bytes-left bytes-needed)
             (decode-err 'end-of-input-in-character))
           (case bytes-needed
             (1 (setf code byte0))
             (2 (put-and-check-valid-secondary-bytes byte1)
                (overlong-check #b11000000 #b10111111)
                (overlong-check #b11000001 #b10111111)
                (setf code (logior (ash (ldb (byte 5 0) byte0) 6)
                                   (ldb (byte 6 0) byte1))))
             (3 (put-and-check-valid-secondary-bytes byte1 byte2)
                (overlong-check #b11100000 #b10011111)
                (setf code (logior (ash (ldb (byte 4 0) byte0) 12)
                                   (ash (ldb (byte 6 0) byte1) 6)
                                   (ldb (byte 6 0) byte2)))
                (when (illegal-unicode-code-point code)
                  (decode-err 'illegal-code-point)))
             (4 (put-and-check-valid-secondary-bytes byte1 byte2 byte3)
                (overlong-check #b11110000 #b10001111)
                (setf code (logior (ash (ldb (byte 3 0) byte0) 18)
                                   (ash (ldb (byte 6 0) byte1) 12)
                                   (ash (ldb (byte 6 0) byte2) 6)
                                   (ldb (byte 6 0) byte3)))))
           (funcall output (code-char code)))))))
  (to-octets
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (let ((code (char-code (funcall input))))
     (when (illegal-unicode-code-point code)
       (setf code (char-code (funcall error-fn 'illegal-character))))
     (cond 
       ((< code #x80)
        (funcall output code))
       ((< code #x800)
        (funcall output (logior #xC0 (ldb (byte 5 6) code)))
        (funcall output (logior #x80 (ldb (byte 6 0) code))))
       ((< code #x10000)
        (funcall output (logior #xE0 (ldb (byte 4 12) code)))
        (funcall output (logior #x80 (ldb (byte 6 6) code)))
        (funcall output (logior #x80 (ldb (byte 6 0) code))))
       ((< code #x200000)
        (funcall output (logior #xF0 (ldb (byte 3 18) code)))
        (funcall output (logior #x80 (ldb (byte 6 12) code)))
        (funcall output (logior #x80 (ldb (byte 6 6) code)))
        (funcall output (logior #x80 (ldb (byte 6 0) code))))))))

(define-external-format :utf-16 (:utf16 :utf-16be :utf16be) 2
  (to-char
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (block utf-16-decode
     (flet ((read-word ()
              (+ (ash (funcall input) 8) (funcall input)))
            (decode-err (sym)
              (return-from utf-16-decode
                (funcall output (funcall error-fn sym)))))
       (macrolet ((put-word (word bytes-needed)
                    `(progn (when (> ,bytes-needed bytes-left)
                              (decode-err 'end-of-input-in-character))
                            (setf ,word (read-word)))))
         (let ((code 0) (w0 0) (w1 0))
           (declare (type (unsigned-byte 32) code)
                    (type (unsigned-byte 16) w0 w1))
           (put-word w0 2)
           (cond ((not (<= #xD800 w0 #xDFFF))
                  (setf code w0))
                 ((> w0 #xDBFF)
                  (decode-err 'invalid-starter-octet))
                 (t (put-word w1 4)
                    (if (<= #xDC00 w1 #xDFFF)
                        (setf code (+ (ash (ldb (byte 10 0) w0) 10)
                                      (ldb (byte 10 0) w1)
                                      #x10000))
                        (decode-err 'invalid-continuation-octet))))
           (funcall output (code-char code)))))))
  (to-octets
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (flet ((write-word (word)
            (funcall output (ldb (byte 8 8) word))
            (funcall output (ldb (byte 8 0) word))))
     (let ((code (char-code (funcall input))))
       (when (illegal-unicode-code-point code)
         (setf code (char-code (funcall error-fn 'illegal-character))))
       (cond ((< code #x10000)
              (write-word code))
             (t (decf code #x10000)
                (write-word (logior #xD800 (ldb (byte 10 10) code)))
                (write-word (logior #xDC00 (ldb (byte 10 0) code)))))))))

(define-external-format :utf-16le (:utf16le) 2
  (to-char
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (block utf-16-decode
     (flet ((read-word ()
              (+ (funcall input) (ash (funcall input) 8)))
            (decode-err (sym)
              (return-from utf-16-decode
                (funcall output (funcall error-fn sym)))))
       (macrolet ((put-word (word bytes-needed)
                    `(progn (when (> ,bytes-needed bytes-left)
                              (decode-err 'end-of-input-in-character))
                            (setf ,word (read-word)))))
         (let ((code 0) (w0 0) (w1 0))
           (declare (type (unsigned-byte 32) code)
                    (type (unsigned-byte 16) w0 w1))
           (put-word w0 2)
           (cond ((not (<= #xD800 w0 #xDFFF))
                  (setf code w0))
                 ((> w0 #xDBFF)
                  (decode-err 'invalid-starter-octet))
                 (t (put-word w1 4)
                    (if (<= #xDC00 w1 #xDFFF)
                        (setf code (+ (ash (ldb (byte 10 0) w0) 10)
                                      (ldb (byte 10 0) w1)
                                      #x10000))
                        (decode-err 'invalid-continuation-octet))))
           (funcall output (code-char code)))))))
  (to-octets
   (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
   (flet ((write-word (word)
            (funcall output (ldb (byte 8 0) word))
            (funcall output (ldb (byte 8 8) word))))
     (let ((code (char-code (funcall input))))
       (when (illegal-unicode-code-point code)
         (setf code (char-code (funcall error-fn 'illegal-character))))
       (cond ((< code #x10000)
              (write-word code))
             (t (decf code #x10000)
                (write-word (logior #xD800 (ldb (byte 10 10) code)))
                (write-word (logior #xDC00 (ldb (byte 10 0) code)))))))))

;;;
;;;
;;; CONVERSION FUNCTIONS
;;;
;;;

;;
;; OCTETS-TO-CHAR
;;

(defmacro octets-to-char (external-format input output error-fn bytes-left)
  `(funcall (ef-octets-to-char ,external-format) ,input ,output ,error-fn
            ,bytes-left))

(defun read-replacement-char ()
  (format *query-io* "Enter a replacement character(evaluated): ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun %octets-to-string (buffer string start end ef &optional max-char-num)
  (declare (type et:foreign-pointer buffer)
           (type buffer-index start end)
           (type external-format ef)
           (type (or null signed-byte) max-char-num)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (unless max-char-num (setf max-char-num -1))
  (let ((ptr start)
        (pos -1)
        (char-count -1)
        oldpos oldptr)
    (tagbody
       (flet ((input ()
                (prog1 (cffi:mem-aref buffer :uint8 ptr) (incf ptr)))
              (output (char)
                (setf (char string (incf pos)) char))
              (error-fn (symbol)
                (restart-case
                    (error symbol :array buffer
                           :start start :end end
                           :position oldptr
                           :external-format (ef-name ef))
                  (use-value (s)
                    :report "Supply a replacement character."
                    :interactive read-replacement-char
                    s)
                  (use-standard-unicode-replacement ()
                    :report "Use standard UCS replacement character"
                    (code-char #xFFFD))
                  (stop-decoding ()
                    :report "Stop decoding and return to last good offset."
                    (setf pos oldpos)
                    (go :exit)))))
         (loop :while (and (< ptr end)
                           (/= (incf char-count) max-char-num))
            :do (setf oldpos pos
                      oldptr ptr)
            (octets-to-char ef #'input #'output #'error-fn (- end ptr))))
     :exit
       (return-from %octets-to-string (values (1+ pos) (- ptr start))))))

(defun octets-to-string (octets
                         &key (start 0) end
                         (external-format :default)
                         (auto-correct nil))
  (setf octets (coerce octets '(simple-array octet (*))))
  (check-type start buffer-index)
  (check-type end (or null buffer-index))
  (let ((ef (find-external-format external-format))
        (end (or end (length octets)))
        (string nil))
    (assert (<= start end))
    (setf string (make-string (- end start)))
    (cffi:with-pointer-to-vector-data (octets-ptr octets)
      (let ((pos (if auto-correct
                     (handler-bind ((octet-decoding-error
                                     #'(lambda (error)
                                         (declare (ignore error))
                                         (invoke-restart 'use-value #\?))))
                       (%octets-to-string octets-ptr string start end ef))
                     (%octets-to-string octets-ptr string start end ef))))
        (shrink-vector string pos)))))

;;
;; CHAR-TO-OCTETS
;;

(defmacro char-to-octets (ef input output error-fn chars-left)
  `(funcall (ef-char-to-octets ,ef) ,input ,output ,error-fn
            ,chars-left))

(defun string-to-octets (string &key (start 0) end
                         (external-format :default)
                         adjust-factor)
  (declare (type string string)
           (type buffer-index start)
           (type (or null buffer-index) end)
           (type (or null real) adjust-factor)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((ef (find-external-format external-format))
         (buffer (make-array (1+ (length string))
                             :element-type 'octet
                             :adjustable t))
         (adjust-threshold (length string))
         (ptr start)
         (pos -1)
         oldpos oldptr)
    (setf adjust-factor (if (and adjust-factor (<= 1 adjust-factor 4))
                            adjust-factor
                            (ef-octet-size ef))
          end (or end (length string)))
    (tagbody
       (flet ((input ()
                (prog1 (char string ptr) (incf ptr)))
              (output (octet)
                (setf (aref buffer (incf pos)) octet)
                (when (= pos adjust-threshold)
                  (setf adjust-threshold (truncate (* adjust-factor (1+ pos))))
                  (setf buffer (adjust-array buffer adjust-threshold))))
              (error-fn (symbol)
                (restart-case
                    (error symbol :array buffer
                           :start start :end end
                           :position oldptr
                           :external-format (ef-name ef))
                  (use-value (s)
                    :report "Supply a replacement character."
                    :interactive read-replacement-char
                    s)
                  (use-standard-unicode-replacement ()
                    :report "Use standard UCS replacement character"
                    (code-char #xFFFD))
                  (stop-decoding ()
                    :report "Stop decoding and return to last good offset."
                    (setf pos oldpos)
                    (go :exit)))))
         (loop :while (< ptr end)
            :do (setf oldpos pos
                      oldptr ptr)
            (char-to-octets ef #'input #'output #'error-fn (- end ptr))))
     :exit (return-from string-to-octets (shrink-vector buffer (1+ pos))))))
