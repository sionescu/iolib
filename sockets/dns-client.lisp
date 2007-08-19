;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; dns-client.lisp --- Thread-safe DNS client.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :net.sockets)

;;;; Constants

(defconstant +opcode-standard+ 0)

(define-constant +query-type-map
    '((:a     .   1)
      (:ns    .   2)
      (:cname .   5)
      (:soa   .   6)
      (:wks   .  11)
      (:ptr   .  12)
      (:hinfo .  13)
      (:mx    .  15)
      (:txt   .  16)
      (:aaaa  .  28)
      (:any   . 255))
  :test #'equal)

(defun query-type-number (id)
  (cdr (assoc id +query-type-map)))

(defun query-type-id (number)
  (car (rassoc number +query-type-map)))

(defun valid-type-p (id)
  (query-type-number id))

(define-constant +query-class-map
    '((:in  .   1)
      (:any . 255))
  :test #'equal)

(defun query-class-number (id)
  (cdr (assoc id +query-class-map)))

(defun query-class-id (number)
  (car (rassoc number +query-class-map)))

(define-constant +rcode-map
    '((:no-error        . 0)
      (:format-error    . 1)
      (:server-failure  . 2)
      (:name-error      . 3)
      (:not-implemented . 4)
      (:refused         . 5))
  :test #'equal)

(defun rcode-number (id)
  (cdr (assoc id +rcode-map)))

(defun rcode-id (number)
  (car (rassoc number +rcode-map)))

(defconstant +dns-datagram-size+ 512)

;;;; Dynamic Buffer

(deftype octet ()
  `(unsigned-byte 8))

(defclass dynamic-output-buffer ()
  ((sequence :initform nil    :reader buffer-sequence)
   (length   :initform 0      :reader buffer-length)
   (size     :initarg :size   :reader buffer-size))
  (:default-initargs :size +dns-datagram-size+))

(defmethod initialize-instance :after ((buffer dynamic-output-buffer)
                                       &key (size 50))
  (setf (slot-value buffer 'sequence)
        (make-array size :element-type 'octet
                    :adjustable t :fill-pointer 0)))

(defun ub16-to-vector (value)
  (vector (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun ub32-to-vector (value)
  (vector (ldb (byte 8 32) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defgeneric write-vector (buffer vector))

(defmethod write-vector :before ((buffer dynamic-output-buffer)
                                 (vector array))
  (with-slots (sequence length size) buffer
    (let ((vector-length (length vector)))
      (when (< size (+ length vector-length))
        (let ((newsize (+ size vector-length 50)))
          (setf sequence (adjust-array sequence newsize))
          (setf size newsize))))))

(defmethod write-vector ((buffer dynamic-output-buffer)
                         (vector array))
  (with-slots (sequence length) buffer
    (let ((vector-length (length vector)))
      (incf (fill-pointer sequence) vector-length)
      (replace sequence vector :start1 length)
      (incf length vector-length)))
  buffer)

(defgeneric write-unsigned-8 (buffer vector))
(defmethod write-unsigned-8 ((buffer dynamic-output-buffer)
                             (value integer))
  (write-vector buffer (vector value)))

(defgeneric write-unsigned-16 (buffer vector))
(defmethod write-unsigned-16 ((buffer dynamic-output-buffer)
                              (value integer))
  (write-vector buffer (ub16-to-vector value)))

(defgeneric write-unsigned-32 (buffer vector))
(defmethod write-unsigned-32 ((buffer dynamic-output-buffer)
                              (value integer))
  (write-vector buffer (ub32-to-vector value)))

(defmacro with-output-buffer (var &body body)
  `(let ((,var (make-instance 'dynamic-output-buffer)))
     ,@body
     ,var))

(defclass dynamic-input-buffer ()
  ((sequence :initform nil :initarg :sequence :reader buffer-sequence)
   (position :initform 0   :reader buffer-position)
   (size     :reader buffer-size)))

(defmethod initialize-instance :after ((buffer dynamic-input-buffer) &key size)
  (with-slots (sequence (seq-size size)) buffer
    (setf seq-size (or size (length sequence)))
    (cond
      ((null sequence)
       (setf sequence (make-array 0 :element-type 'octet :adjustable t
                                  :initial-contents sequence)))
      ((not (and (adjustable-array-p sequence)
                 (typep sequence '(vector octet))))
       (setf sequence (make-array seq-size
                                  :element-type 'octet :adjustable t
                                  :displaced-to sequence))))))

(define-condition input-buffer-error (error) ())

(define-condition input-buffer-scarcity (input-buffer-error)
  ((bytes-requested :initarg :requested :reader bytes-requested)
   (bytes-remaining :initarg :remaining :reader bytes-remaining))
  (:documentation
   "Signals that an INPUT-BUFFER contains less unread bytes than requested."))

(define-condition input-buffer-eof (input-buffer-scarcity) ()
  (:documentation
   "Signals that an INPUT-BUFFER contains no more unread bytes."))

(define-condition input-buffer-index-out-of-bounds (input-buffer-error) ()
  (:documentation
   "Signals that BUFFER-SEEK on an INPUT-BUFFER was passed an
invalid offset."))

(defgeneric buffer-seek (buffer offset))
(defmethod buffer-seek ((buffer dynamic-input-buffer) offset)
  (check-type offset unsigned-byte "a non-negative value")
  (with-slots (sequence size position) buffer
    (if (> offset (1- size))
        (error 'input-buffer-index-out-of-bounds)
        (setf position offset))))

(defgeneric buffer-append (buffer vector))
(defmethod buffer-append ((buffer dynamic-input-buffer)
                          vector)
  (with-slots (sequence size) buffer
    (when (plusp (length vector))
      (let ((oldsize size)
            (newsize (+ (length sequence)
                        (length vector))))
        (setf sequence (adjust-array sequence newsize))
        (replace sequence vector :start1 oldsize)
        (setf size newsize)))))

(defgeneric bytes-unread (buffer))
(defmethod bytes-unread ((buffer dynamic-input-buffer))
  (with-slots (position size) buffer
    (- size position)))

(defgeneric check-if-enough-bytes (buffer length &key check-all))
(defmethod check-if-enough-bytes ((buffer dynamic-input-buffer)
                                  length &key (check-all t))
  (let ((bytes-unread (bytes-unread buffer)))
    (cond
      ((and (zerop bytes-unread)
            (plusp length))
       (error 'input-buffer-eof
              :requested length
              :remaining bytes-unread))
      ((and check-all
            (< bytes-unread length))
       (error 'input-buffer-scarcity
              :requested length
              :remaining bytes-unread)))
    t))

(defun read-ub16-from-vector (vector position)
  (+ (ash (aref vector position) 8)
     (aref vector (1+ position))))

(defun read-ub32-from-vector (vector position)
  (+ (ash (aref vector position) 24)
     (ash (aref vector (1+ position)) 16)
     (ash (aref vector (+ position 2)) 8)
     (aref vector (+ position 3))))

(defgeneric read-vector (buffer length &key read-all))
(defmethod read-vector ((buffer dynamic-input-buffer)
                        length &key (read-all t))
  (let* ((bytes-to-read
          (min (bytes-unread buffer) length))
         (newvector
          (make-array bytes-to-read :element-type 'octet)))
    (check-if-enough-bytes buffer length :check-all read-all)
    (with-slots (sequence position) buffer
      (replace newvector sequence :start2 position)
      (incf position bytes-to-read))
    newvector))

(defgeneric read-unsigned-8 (buffer))
(defmethod read-unsigned-8 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 1)
  (with-slots (sequence position) buffer
    (prog1
        (aref sequence position)
      (incf position))))

(defgeneric read-unsigned-16 (buffer))
(defmethod read-unsigned-16 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 2)
  (with-slots (sequence position) buffer
    (prog1
        (read-ub16-from-vector sequence position)
      (incf position 2))))

(defgeneric read-unsigned-32 (buffer))
(defmethod read-unsigned-32 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 4)
  (with-slots (sequence position) buffer
    (prog1
        (read-ub32-from-vector sequence position)
      (incf position 4))))

(defmacro with-input-buffer ((var) &body body)
  `(let ((,var (make-instance 'dynamic-input-buffer)))
     ,@body
     ,var))

;;;; Etc Files

(defun load-file (path)
  (with-open-file (fin path)
    (let ((big-string (make-string (file-length fin))))
      (read-sequence big-string fin)
      big-string)))

(defun space-char-p (char)
  (declare (type character char))
  (or (char-equal char #\Space)
      (char-equal char #\Tab)))

(defun split-string-by-spaces (string &key (start 0) end empty-seqs)
  (declare (type string string)
           (type unsigned-byte start)
           (type (or unsigned-byte null) end))
  (let ((substring-length (or end (length string))))
    (assert (>= substring-length start))
    (loop with substr-start = (1- start) and substr-end = (1- start)
          with dummy-char = #\Space
          for index upto substring-length
          for char = (if (eql index substring-length)
                         dummy-char
                         (char string index))
          when (and (space-char-p char)
                    (setf substr-start (1+ substr-end)
                          substr-end   index)
                    (or (> substr-end substr-start) empty-seqs))
          collect (subseq string substr-start substr-end))))

(defun search-in-etc-file (path predicate &optional (match-all t))
  (let ((file (load-file path))
        results)
    (with-input-from-string (string-stream file)
      (loop for line = (read-line string-stream nil nil)
            for comment-start = (or (position #\# line)
                                    (length line))
            while line do
            (destructuring-bind (&optional col1 col2 &rest other-cols)
                (split-string-by-spaces
                 line :empty-seqs nil :end comment-start)
              (when col2                ; skip invalid lines
                (let ((result (funcall predicate col1 col2 other-cols)))
                  (when result
                    (push result results)
                    (unless match-all
                      (loop-finish))))))
            finally (setf results (nreverse results))))
    results))

(defun vector-ipv6-good-p (vector ipv6)
  (when vector
    (let ((len (length vector)))
      (case ipv6
        (:ipv6 (eql len 8))
        ((nil) (eql len 4))
        (otherwise t)))))

(defun search-etc-hosts-ip (file ip ipv6)
  (car
   (search-in-etc-file
    file
    (lambda (col1 col2 other-cols)
      (let ((vector (string-address-to-vector col1)))
        (when (and (vector-ipv6-good-p vector ipv6)
                   (vector-equal vector ip))
          (let ((host
                 (make-host col2 (make-address vector) other-cols)))
            (if (eql ipv6 t)
                (map-host-ipv4-addresses-to-ipv6 host)
                host)))))
    nil)))

(defun merge-lines-into-one-host (lines ipv6)
  (flet ((pushnew-alias (alias place cname)
           (when (string-not-equal alias cname)
             (pushnew alias place :test #'string-equal)
             place)))
    (let (ips aliases host)
      (destructuring-bind (first-ip cname first-aliases) (car lines)
        (setf ips (list first-ip))
        (mapc (lambda (alias)
                (setf aliases (pushnew-alias alias aliases cname)))
              first-aliases)
        (mapc (lambda (line)
                (destructuring-bind (ip alias more-aliases) line
                  (pushnew ip ips)
                  (mapc (lambda (alias)
                          (setf aliases (pushnew-alias alias aliases cname)))
                        (cons alias more-aliases))))
              (cdr lines))
        (setf host (make-host cname
                              (mapcar #'make-address (nreverse ips))
                              (nreverse aliases)))
        (if (eql ipv6 t)
            (map-host-ipv4-addresses-to-ipv6 host)
            host)))))

(defun search-etc-hosts-name (file name ipv6)
  (let ((lines (search-in-etc-file
                file
                (lambda (col1 col2 other-cols)
                  (let ((vector (string-address-to-vector col1)))
                    (when (and (vector-ipv6-good-p vector ipv6)
                               (or (string-equal name col2)
                                   (member name other-cols
                                           :test #'string-equal)))
                      (list vector col2 other-cols))))
                t)))
    (when lines
      (merge-lines-into-one-host lines ipv6))))

;;; Only parses NAMESERVER, DOMAIN and SEARCH directives, for now.
(defun search-etc-resolv-conf (file)
  (with-open-file (s file :direction :input)
    (let (nameservers domain search-domain)
      (loop for line = (read-line s nil nil) while line do
            (let ((tokens (split-sequence #\Space line)))
              ;; case sensitive?
              (switch ((first tokens) :test #'string-equal)
                ("nameserver" (ignore-some-conditions (parse-error)
                                (push (ensure-address (second tokens))
                                      nameservers)))
                ("domain" (setq domain (second tokens)))
                ("search" (setq search-domain (second tokens))))))
      (values (nreverse nameservers) domain search-domain))))

;;;; DNS Queries

(defclass dns-message ()
  ((id    :initform 0 :initarg :id    :accessor dns-message-id)
   (flags :initform 0 :initarg :flags :accessor dns-message-flags)
   (decoded-flags :reader decoded-flags)
   (qdcount :initarg :qdcount :reader dns-message-question-count)
   (ancount :initarg :ancount :reader dns-message-answer-count)
   (nscount :initarg :nscount :reader dns-message-authority-count)
   (arcount :initarg :arcount :reader dns-message-additional-count)
   (question   :reader dns-message-question)
   (answer     :reader dns-message-answer)
   (authority  :reader dns-message-authority)
   (additional :reader dns-message-additional))
  (:default-initargs :qdcount 1 :ancount 0 :nscount 0 :arcount 0))

(defmacro define-flags-bitfield (name offset length &optional (type :integer))
  (let ((method-name (format-symbol t "~A-FIELD" name)))
    `(progn
       (defgeneric ,method-name (message))
       (defmethod ,method-name ((message dns-message))
         ,(ecase type
            (:integer `(ldb (byte ,length ,offset)
                            (dns-message-flags message)))
            (:boolean `(logbitp ,offset (dns-message-flags message)))
            (:rcode `(rcode-id
                      (ldb (byte ,length ,offset)
                           (dns-message-flags message))))))
       (defgeneric (setf ,method-name) (value message))
       (defmethod (setf ,method-name) (value (message dns-message))
         ,(ecase type
            (:integer `(setf (ldb (byte ,length ,offset)
                                  (dns-message-flags message))
                            value))
            (:boolean `(setf (ldb (byte ,length ,offset)
                                  (dns-message-flags message))
                            (lisp->c-bool value)))
            (:rcode `(setf (ldb (byte ,length ,offset)
                                (dns-message-flags message))
                          (rcode-number value))))))))

(define-flags-bitfield response 15 1 :boolean)
(define-flags-bitfield opcode 11 4 :integer)
(define-flags-bitfield authoritative 10 1 :boolean)
(define-flags-bitfield truncated 9 1 :boolean)
(define-flags-bitfield recursion-desired 8 1 :boolean)
(define-flags-bitfield recursion-available 7 1 :boolean)
(define-flags-bitfield rcode 0 4 :rcode)

(defgeneric decode-flags (message))
(defmethod decode-flags ((msg dns-message))
  (let (flags)
    (push (if (response-field msg) :response :query) flags)
    (push (if (eql (opcode-field msg) +opcode-standard+)
              :opcode-standard :opcode-unknown)
          flags)
    (when (authoritative-field msg) (push :authoritative flags))
    (when (truncated-field msg) (push :truncated flags))
    (when (recursion-desired-field msg) (push :recursion-desired flags))
    (when (recursion-available-field msg) (push :recursion-available flags))
    (push (or (rcode-field msg) :rcode-unknown) flags)
    (nreverse flags)))

(defmethod initialize-instance :after ((msg dns-message) &key
                                       (qdcount 0) (ancount 0)
                                       (nscount 0) (arcount 0))
  (with-slots (id flags decoded-flags question answer authority additional) msg
    (setf decoded-flags (decode-flags msg))
    (setf question (make-array qdcount :adjustable t :fill-pointer 0))
    (setf answer (make-array ancount :adjustable t :fill-pointer 0))
    (setf authority (make-array nscount :adjustable t :fill-pointer 0))
    (setf additional (make-array arcount :adjustable t :fill-pointer 0))))

(defclass dns-record ()
  ((name  :initarg :name  :accessor dns-record-name)
   (type  :initarg :type  :accessor dns-record-type)
   (class :initarg :class :accessor dns-record-class)))

(defmethod initialize-instance :after ((record dns-record) &key)
  (with-slots (name type class) record
    (check-type name string "a string")
    (check-type type (satisfies valid-type-p) "a valid record type")
    (check-type class (member :in) "a valid record class")))

(defclass dns-question (dns-record) ())

(defmethod initialize-instance :after ((record dns-question) &key)
  (with-slots (name) record
    (let ((name-length (length name)))
      (when (char-not-equal (aref name (1- name-length))
                            #\.)
        (setf name (concatenate 'string name (string #\.)))))))

;;;; Constructors

(defun make-question (qname qtype qclass)
  (make-instance 'dns-question
                 :name qname
                 :type qtype
                 :class qclass))

(defun make-query (id question &optional recursion-desired)
  (let ((msg (make-instance 'dns-message :id id)))
    (setf (opcode-field msg) +opcode-standard+)
    (setf (recursion-desired-field msg) recursion-desired)
    (vector-push-extend question (dns-message-question msg))
    msg))

;;;; Output Record

(defgeneric write-dns-string (buffer string))
(defmethod write-dns-string ((buffer dynamic-output-buffer) (string string))
  (write-unsigned-8 buffer (length string))
  ;; Probably want to use punnycode here.
  (write-vector buffer (babel:string-to-octets string :encoding :ascii)))

(defun domain-name-to-dns-format (domain-name)
  (let* ((octets (babel:string-to-octets domain-name :encoding :ascii))
         (tmp-vec (make-array (1+ (length octets)) :element-type 'octet)))
    (replace tmp-vec octets :start1 1)
    (let ((vector-length (length tmp-vec)))
      (loop for start-off = 1 then (1+ end-off)
            for end-off = (or (position (char-code #\.) tmp-vec
                                        :start start-off)
                             vector-length)
            do (setf (aref tmp-vec (1- start-off)) (- end-off start-off))
            when (>= end-off vector-length) do (loop-finish)))
    tmp-vec))

(defgeneric write-domain-name (buffer name))
(defmethod write-domain-name ((buffer dynamic-output-buffer)
                              (domain-name string))
  (write-vector buffer (domain-name-to-dns-format domain-name)))

(defgeneric write-record (buffer record))
(defmethod write-record ((buffer dynamic-output-buffer)
                         (record dns-question))
  (with-slots (name type class) record
    (write-domain-name buffer name)
    (write-unsigned-16 buffer (query-type-number type))
    (write-unsigned-16 buffer (query-class-number class))))

(defgeneric write-message-header (buffer message))
(defmethod write-message-header ((buffer dynamic-output-buffer)
                                 (message dns-message))
  (with-slots (id flags question answer authority additional)
      message
    (write-unsigned-16 buffer id)
    (write-unsigned-16 buffer flags)
    (write-unsigned-16 buffer (length question))
    (write-unsigned-16 buffer (length answer))
    (write-unsigned-16 buffer (length authority))
    (write-unsigned-16 buffer (length additional))))

(defgeneric write-dns-message (message))
(defmethod write-dns-message ((message dns-message))
  (with-slots (question) message
    (with-output-buffer buffer
      (write-message-header buffer message)
      (write-record buffer (aref question 0)))))

;;;; DNS Response

(defclass dns-rr (dns-record)
  ((ttl  :initarg :ttl  :accessor dns-rr-ttl)
   (data :initarg :data :accessor dns-rr-data)))

(defmethod initialize-instance :after ((rr dns-rr) &key)
  (with-slots (ttl) rr
    (check-type ttl (unsigned-byte 32) "a valid TTL")))

(defgeneric add-question (message question))
(defmethod add-question ((message dns-message)
                         (question dns-question))
  (vector-push-extend question (dns-message-question message)))

(defgeneric add-answer-rr (message record))
(defmethod add-answer-rr ((message dns-message)
                          (record dns-rr))
  (vector-push-extend record (dns-message-answer message)))

(defgeneric add-authority-rr (message record))
(defmethod add-authority-rr ((message dns-message)
                             (record dns-rr))
  (vector-push-extend record (dns-message-authority message)))

(defgeneric add-additional-rr (message record))
(defmethod add-additional-rr ((message dns-message)
                              (record dns-rr))
  (vector-push-extend record (dns-message-additional message)))


(define-condition dns-message-error (error) ()
  (:documentation
   "Signaled when a format error is encountered while parsing a DNS message"))

(defgeneric read-dns-string (buffer))
(defmethod read-dns-string ((buffer dynamic-input-buffer))
  (let ((length (read-unsigned-8 buffer)))
    (babel:octets-to-string (read-vector buffer length) :encoding :ascii)))

(defun read-dns-pointer-recursively (sequence position
                                     &optional (depth 5))
  (when (or (<= depth 0)                          ; too deep recursion
            (>= position (length sequence)))      ; invalid offset
    (error 'dns-message-error))
  (let* ((value (aref sequence position))
         (ms2bits (logand value #xC0)))
    (cond
      ;; it's not a pointer
      ((zerop ms2bits) (cons position (< depth 5)))
      ;; it's a pointer
      ((eql ms2bits #xC0)
       ;; there must be at least two bytes to read
       (when (>= position (1+ (length sequence)))
         (error 'dns-message-error))
       (read-dns-pointer-recursively
        sequence
        (logand (read-ub16-from-vector sequence position)
                (lognot #xC000))
        (1- depth)))
      ;; the most significant 2 bits are either 01 or 10
      (t (error 'dns-message-error)))))

(defun join (connector strings)
  (concatenate 'string (car strings)
               (reduce (lambda (str1 str2)
                         (concatenate 'string str1 connector str2))
                       (cdr strings)
                       :initial-value "")))

(defgeneric dns-domain-name-to-string (buffer))
(defmethod dns-domain-name-to-string ((buffer dynamic-input-buffer))
  (let (string offset pointer-seen)
    (values
     (join "."
           (loop for (pointer . rec) = (read-dns-pointer-recursively
                                        (buffer-sequence buffer)
                                        (buffer-position buffer))
                 do (progn
                      (when (not pointer-seen)
                        (if rec
                            (progn
                              (setf pointer-seen t)
                              (setf offset (+ (buffer-position buffer) 2)))
                            (setf offset (+ (buffer-position buffer) 1))))
                      (buffer-seek buffer pointer)
                      (setf string (read-dns-string buffer)))
                 collect string
                 until (string= string "")))
     offset)))

(defgeneric read-domain-name (buffer))
(defmethod read-domain-name ((buffer dynamic-input-buffer))
  (with-slots (sequence position) buffer
    (multiple-value-bind (string offset)
        (dns-domain-name-to-string buffer)
      (setf position offset)
      string)))

(defgeneric read-question (buffer))
(defmethod read-question ((buffer dynamic-input-buffer))
  (let ((name (read-domain-name buffer))
        (type (query-type-id (read-unsigned-16 buffer)))
        (class (query-class-id (read-unsigned-16 buffer))))
    (make-question name type class)))

(defgeneric read-rr-data (buffer type class length))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :a)) (class (eql :in))
                         resource-length)
  (unless (= resource-length 4)
    (error 'dns-message-error))
  (let ((address (make-array 4 :element-type 'octet)))
    (dotimes (i 4)
      (setf (aref address i) (read-unsigned-8 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :aaaa)) (class (eql :in))
                         resource-length)
  (unless (= resource-length 16)
    (error 'dns-message-error))
  (let ((address (make-array 8 :element-type '(unsigned-byte 16))))
    (dotimes (i 8)
      (setf (aref address i) (read-unsigned-16 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :cname)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer))            ; CNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :hinfo)) (class (eql :in))
                         resource-length)
  (list (read-dns-string buffer)        ; CPU
        (read-dns-string buffer)))      ; OS

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :mx)) (class (eql :in))
                         resource-length)
  (list (read-unsigned-16 buffer)       ; PREFERENCE
        (read-domain-name buffer)))     ; EXCHANGE

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :ns)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer))            ; NSDNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :ptr)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer))            ; PTRDNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :soa)) (class (eql :in))
                         resource-length)
  (list (read-domain-name buffer)       ; MNAME
        (read-domain-name buffer)       ; RNAME
        (read-unsigned-32 buffer)       ; SERIAL
        (read-unsigned-32 buffer)       ; REFRESH
        (read-unsigned-32 buffer)       ; RETRY
        (read-unsigned-32 buffer)       ; EXPIRE
        (read-unsigned-32 buffer)))     ; MINIMUM

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :txt)) (class (eql :in))
                         resource-length)
  (loop for string = (read-dns-string buffer) ; TXT-DATA
        for total-length = (1+ (length string))
        then (+ total-length 1 (length string))
        collect string
        until (>= total-length resource-length)
        finally (when (> total-length resource-length)
                  (error 'dns-message-error))))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         type class resource-length)
  (error 'dns-message-error))

(defgeneric read-dns-rr (buffer))
(defmethod read-dns-rr ((buffer dynamic-input-buffer))
  (let* ((name (read-domain-name buffer))
         (type (query-type-id (read-unsigned-16 buffer)))
         (class (query-class-id (read-unsigned-16 buffer)))
         (ttl (read-unsigned-32 buffer))
         (rdlen (read-unsigned-16 buffer))
         (rdata (read-rr-data buffer type class rdlen)))
    (make-instance 'dns-rr
                   :name name
                   :type type
                   :class class
                   :ttl ttl
                   :data rdata)))

(defgeneric read-message-header (buffer))
(defmethod read-message-header ((buffer dynamic-input-buffer))
  (let ((id (read-unsigned-16 buffer))
        (flags (read-unsigned-16 buffer))
        (qdcount (read-unsigned-16 buffer))
        (ancount (read-unsigned-16 buffer))
        (nscount (read-unsigned-16 buffer))
        (arcount (read-unsigned-16 buffer)))
    (make-instance 'dns-message
                   :id id :flags flags
                   :qdcount qdcount :ancount ancount
                   :nscount nscount :arcount arcount)))

(defgeneric read-dns-message (buffer))
(defmethod read-dns-message ((buffer dynamic-input-buffer))
  (let ((msg (read-message-header buffer)))
    (with-slots (qdcount ancount nscount arcount) msg
      (loop for i below (dns-message-question-count msg)
            for q = (read-question buffer)
            do (add-question msg q))
      (loop for i below (dns-message-answer-count msg)
            for rr = (read-dns-rr buffer)
            do (add-answer-rr msg rr))
      (loop for i below (dns-message-authority-count msg)
            for rr = (read-dns-rr buffer)
            do (add-authority-rr msg rr))
      (loop for i below (dns-message-additional-count msg)
            for rr = (read-dns-rr buffer)
            do (add-additional-rr msg rr)))
    msg))

;;;; Do Query

(defvar *dns-nameservers* nil
  "List of the DNS nameservers to use.")

(defvar *dns-recursion-desired* t
  "Whether the \"RECURSION-DESIRED\" field should be set ot not.")

(defvar *dns-repeat* 5
  "The number of times a failed query will be retried.")

(defvar *dns-timeout* 5
  "Timeout for DNS queries in seconds.")

(defvar *dns-domain* nil
  "The current machine's domain.")

(defvar *dns-search-domain* nil
  "A domain name to be appended to the name to be searched when
the latter does not contain dots.")

(defun send-query (socket-type buffer nameserver timeout)
  (let ((socket (make-socket :type socket-type
                             :ipv6 (ipv6-address-p nameserver)))
        (input-buffer (make-array +dns-datagram-size+
                                  :element-type 'octet)))
    (unwind-protect
         (progn
           (connect socket nameserver :port 53)
           (socket-send buffer socket)
           (set-socket-option socket :receive-timeout :sec timeout :usec 0)
           (socket-receive input-buffer socket))
      (close socket))))

(define-constant +max-16-bits+ (1- (expt 2 16)))

(defun prepare-query (name type)
  (let* ((question (make-question name type :in))
         (query (make-query (random +max-16-bits+)
                            question *dns-recursion-desired*)))
    (write-dns-message query)))

(defun reverse-vector (vector)
  (let* ((vector-length (length vector))
         (reverse-vector
          (make-array vector-length
                      :element-type (array-element-type vector))))
    (loop for target-index below vector-length
          for source-index = (1- vector-length) then (1- source-index)
          do (setf (aref reverse-vector target-index)
                   (aref vector source-index)))
    reverse-vector))

(defun ipv4-dns-ptr-name (address)
  (declare (type (simple-array octet (4)) address))
  (concatenate 'string (vector-to-dotted (reverse-vector address))
               ".in-addr.arpa."))

(defun ipv6-vector-to-dotted (vector)
  (declare (type (simple-array ub16 (8)) vector))
  (with-standard-io-syntax
    (let ((*print-base* 16))
      (with-output-to-string (dotted-address)
        (loop for index below (length vector)
              for element = (aref vector index) do
              (when (plusp index)
                (princ #\. dotted-address))
              (princ (ldb (byte 4  0) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4  4) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4  8) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4 12) element) dotted-address))))))

(defun ipv6-dns-ptr-name (address)
  (declare (type (simple-array ub16 (8)) address))
  (concatenate 'string (ipv6-vector-to-dotted (reverse-vector address))
               ".ip6.arpa."))

(defun dns-ptr-name (address)
  (multiple-value-bind (vector address-type)
      (address-to-vector address)
    (when (null address)
      (error "The argument is not a valid IP address"))
    (ecase address-type
      (:ipv4 (ipv4-dns-ptr-name vector))
      (:ipv6 (ipv6-dns-ptr-name vector)))))

;;;; Resource Record Decoding

(defgeneric %decode-rr (rr type class))

(defmethod %decode-rr ((rr dns-rr) (type (eql :cname)) class)
  (declare (ignore class))
  (let ((cname (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (subseq cname 0 (1- (length cname))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :a)) (class (eql :in)))
  (let ((address (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (make-address address))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :aaaa)) (class (eql :in)))
  (let ((address (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (make-address address))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :ptr)) class)
  (declare (ignore class))
  (let ((name (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (subseq name 0 (1- (length name))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :mx)) class)
  (declare (ignore class))
  (destructuring-bind (preference name) (dns-rr-data rr)
    (cons (dns-rr-ttl rr)
          (cons preference
                (subseq name 0 (1- (length name)))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :txt)) class)
  (declare (ignore class))
  (cons (dns-rr-ttl rr) (dns-rr-data rr)))

(defun decode-rr (rr)
  (%decode-rr rr (dns-record-type rr) (dns-record-class rr)))

;;;; Response Decoding

(defgeneric %decode-response (dns-message question-type))

(defmethod %decode-response :around ((msg dns-message) question-type)
  (let ((return-code (rcode-field msg)))
    (if (eql return-code :no-error) ; no error
        (call-next-method)
        (values return-code))))

(defun decode-a-or-aaaa-response (msg)
  (declare (type dns-message msg))
  (let ((answer (dns-message-answer msg))
        (answer-count (dns-message-answer-count msg))
        (cname nil)
        (first-address-place 0)
        (first-address nil)
        (other-addresses nil))
    ;; when the address is valid(we have at least one answer)
    (when (plusp answer-count)
      ;; we have a CNAME
      (when (eql (dns-record-type (aref answer 0))
                 :cname)
        (setf cname (decode-rr (aref answer 0)))
        (incf first-address-place))
      ;; this means the message actually contains addresses
      (when (> (dns-message-answer-count msg) first-address-place)
        (setf first-address (decode-rr (aref answer first-address-place))))
      (setf other-addresses
            (loop for i from (1+ first-address-place)
                  below (dns-message-answer-count msg)
                  collect (decode-rr (aref answer i)))))
    (values cname first-address other-addresses)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :a)))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :aaaa)))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :ptr)))
  (decode-rr (aref (dns-message-answer msg) 0)))

;; TODO: got a lot to do here
(defmethod %decode-response ((msg dns-message) (question-type (eql :mx)))
  (let ((rr (aref (dns-message-answer msg) 0)))
    (decode-rr rr)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :txt)))
  (decode-rr (aref (dns-message-answer msg) 0)))

(defmethod %decode-response ((msg dns-message) question-type)
  msg)

(defun decode-response (message)
  (%decode-response message (dns-record-type
                             (aref (dns-message-question message) 0))))

;;;; DNS-QUERY

(defun dns-query (name &key (type :a) (nameserver *dns-nameservers*)
                  (repeat *dns-repeat*) (timeout *dns-timeout*)
                  (decode nil) (search nil))
  ;; TODO: implement search
  (declare (ignore search))
  (when (eq type :ptr)
    (setf name (dns-ptr-name name)))
  (let* ((query (prepare-query name type))
         (buffer (buffer-sequence query))
         (bufflen (length buffer))
         (tries-left repeat)
         in-buff bytes-received response tcp-done)
    ;; at the moment only one nameserver is used
    (when (listp nameserver)
      (setf nameserver (car nameserver)))
    (assert nameserver)
    (tagbody
     :start
       (setf tcp-done nil
             response nil)
     :do-any-query
       ;; if the query size fits into a datagram(512 bytes max) do a
       ;; UDP query, otherwise use TCP
       ;; in case of a socket error, try again
       (setf (values in-buff bytes-received)
             (if (> bufflen +dns-datagram-size+)
                 (go :do-tcp-query)
                 (handler-case
                     (send-query :datagram buffer nameserver timeout)
                   (socket-error ()
                     (go :try-again-if-possible)))))
       ;; if no socket error, go parse the response
       (go :parse-response)
     :do-tcp-query
       ;; do a TCP query; in case of a socket error, try again
       (setf (values in-buff bytes-received)
             (handler-case
                 (send-query :stream buffer nameserver timeout)
               (socket-error ()
                 (go :try-again-if-possible))))
       (setf tcp-done t)
     :parse-response
       ;; try to parse the response; in case of a parse error, try again
       (setf response
             (handler-case
                 (read-dns-message
                  (make-instance 'dynamic-input-buffer
                                 :sequence in-buff
                                 :size bytes-received))
               (input-buffer-error ()
                 (go :try-again-if-possible))
               (dns-message-error ()
                 (go :try-again-if-possible))))
       ;; if a truncated response was received by UDP, try TCP
       (when (and (not tcp-done)
                  (truncated-field response))
         (go :do-tcp-query))
     :try-again-if-possible
       (decf tries-left)
       ;; if no response received and there are tries left, try again
       (when (and (not response)
                  (plusp tries-left))
         (go :start))
     :return-response
       (when response
         (return-from dns-query (if decode
                                    (decode-response response)
                                    response)))
     :raise-error
       (error "Could not query nameserver !!"))))

;;;; Low-level Interface

(defvar *hosts-file* "/etc/hosts")
(defvar *resolv-file* "/etc/resolv.conf")

(defgeneric dns-lookup-host (host &key ipv6))

;;; KLUDGE: add caching, don't overwrite the specials mindlessly, etc.
(defmethod dns-lookup-host :before (host &key &allow-other-keys)
  (setf (values *dns-nameservers* *dns-domain* *dns-search-domain*)
        (search-etc-resolv-conf *resolv-file*)))

(defmethod dns-lookup-host ((host string) &key (ipv6 *ipv6*))
  (or (search-etc-hosts-name *hosts-file* host ipv6)
      (dns-query host :type :a)))

(defun dns-lookup-host-ip (vector ipv6)
  (or (search-etc-hosts-ip *hosts-file* vector ipv6)
      (dns-query vector :type :ptr)))

(defmethod dns-lookup-host ((host ipv4-address) &key (ipv6 *ipv6*))
  (dns-lookup-host-ip (address-name host) ipv6))

(defmethod dns-lookup-host ((host ipv6-address) &key (ipv6 *ipv6*))
  (dns-lookup-host-ip (address-name host) ipv6))

(defmethod dns-lookup-host ((host simple-vector) &key (ipv6 *ipv6*))
  (dns-lookup-host (ensure-address host) :ipv6 ipv6))

;;;; High-level Interface

;;; TODO: caching, etc.  Also, verify that this isn't completely
;;; wrong.  It's very likely that it isn't complete because my
;;; knowledge of DNS is almost nil.  --luis

(defun lookup-host (host &key (ipv6 *ipv6*))
  "Looks up a host by name or address.  IPV6 determines the IPv6
behaviour, defaults to *IPV6*."
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (let ((reply (dns-lookup-host host :ipv6 ipv6)))
    (when (typep reply 'host)
      (return-from lookup-host reply))
    ;; or check the :NAME-ERROR flag?
    (cond ((member :name-error (decoded-flags reply))
           (error 'resolver-no-name-error :data nil :message nil))
          ((member :server-failure (decoded-flags reply))
           (error 'resolver-fail-error :data nil :message nil)))
    (flet ((rtd (string)
             ;; remove trailing dot
             (assert (> (length string) 1))
             (assert (char= #\. (char string (1- (length string)))))
             (subseq string 0 (1- (length string)))))
      (loop with aliases = nil and truename = nil and addresses = nil
            for record across (dns-message-answer reply) do
            (case (dns-record-type record)
              (:cname (push (rtd (dns-record-name record)) aliases))
              (:a (setq truename (rtd (dns-record-name record)))
                  (push (ensure-address (dns-rr-data record)) addresses))
              (:ptr (setq truename (rtd (dns-rr-data record)))
                    ;; is this right?
                    (push (ensure-address host) addresses)))
            finally (return (make-host truename addresses aliases))))))
