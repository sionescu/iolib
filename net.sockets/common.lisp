;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; common.lisp --- Various helpers for bsd-sockets.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

;;;; Types

(deftype ipv4-array () '(ub8-sarray 4))
(deftype ipv6-array () '(ub16-sarray 8))

;;;; Byte-swap functions

(defun htons (short)
  #+little-endian
  (logior (ash (logand (the ub16 short) #x00FF) 8)
          (ash (logand (the ub16 short) #xFF00) -8))
  #+big-endian short)

(defun ntohs (short)
  (htons short))

(defun htonl (long)
  #+little-endian
  (logior (ash (logand (the ub32 long) #x000000FF) 24)
          (ash (logand (the ub32 long) #x0000FF00) 8)
          (ash (logand (the ub32 long) #x00FF0000) -8)
          (ash (logand (the ub32 long) #xFF000000) -24))
  #+big-endian long)

(defun ntohl (long)
  (htonl long))

;;;; Conversion between address formats

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type ipv6-array lisp-vec))
  (dotimes (i 8)
    (setf (mem-aref alien-vec :uint16 i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-vector-to-ipv6 (addr)
  (declare (type ipv4-array addr))
  (let ((ipv6addr (make-array 8 :element-type 'ub16
                              :initial-element 0)))
    ;; setting the IPv4 marker
    (setf (aref ipv6addr 5) #xFFFF)
    ;; setting the first two bytes
    (setf (aref ipv6addr 6) (+ (ash (aref addr 0) 8)
                               (aref addr 1)))
    ;; setting the last two bytes
    (setf (aref ipv6addr 7) (+ (ash (aref addr 2) 8)
                               (aref addr 3)))
    (values ipv6addr)))

(defun map-ipv6-vector-to-ipv4 (addr)
  (declare (type ipv6-array addr))
  (let ((ipv4addr (make-array 4 :element-type 'ub8
                              :initial-element 0)))
    (setf (aref ipv4addr 0) (ldb (byte 8 8) (aref addr 6)))
    (setf (aref ipv4addr 1) (ldb (byte 8 0) (aref addr 6)))
    (setf (aref ipv4addr 2) (ldb (byte 8 8) (aref addr 7)))
    (setf (aref ipv4addr 3) (ldb (byte 8 0) (aref addr 7)))
    (values ipv4addr)))

;;; From CLOCC's PORT library.
(defun vector-to-integer (vector)
  "Convert a vector to a 32-bit unsigned integer."
  (coercef vector 'ipv4-array)
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun integer-to-vector (ipaddr)
  "Convert a 32-bit unsigned integer to a vector."
  (check-type ipaddr ub32 "an '(unsigned-byte 32)")
  (let ((vector (make-array 4 :element-type 'ub8)))
    (setf (aref vector 0) (ldb (byte 8 24) ipaddr)
          (aref vector 1) (ldb (byte 8 16) ipaddr)
          (aref vector 2) (ldb (byte 8  8) ipaddr)
          (aref vector 3) (ldb (byte 8  0) ipaddr))
    vector))

(defun in6-addr-to-ipv6-array (in6-addr)
  (let ((vector (make-array 8 :element-type 'ub16)))
    (dotimes (i 8)
      (setf (aref vector i)
            (ntohs (mem-aref in6-addr :uint16 i))))
    vector))

;;;; Constructors for SOCKADDR_* structs

(defun make-sockaddr-in (sin ub8-vector &optional (portno 0))
  (declare (type ipv4-array ub8-vector) (type ub16 portno))
  (bzero sin size-of-sockaddr-in)
  (with-foreign-slots ((family addr port) sin sockaddr-in)
    (setf family af-inet)
    (setf addr (htonl (vector-to-integer ub8-vector)))
    (setf port (htons portno)))
  (values sin))

(defmacro with-sockaddr-in ((var address &optional (port 0)) &body body)
  `(with-foreign-object (,var 'sockaddr-in)
     (make-sockaddr-in ,var ,address ,port)
     ,@body))

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (portno 0))
  (declare (type ipv6-array ub16-vector) (type ub16 portno))
  (bzero sin6 size-of-sockaddr-in6)
  (with-foreign-slots ((family addr port) sin6 sockaddr-in6)
    (setf family af-inet6)
    (copy-simple-array-ub16-to-alien-vector ub16-vector addr)
    (setf port (htons portno)))
  (values sin6))

(defmacro with-sockaddr-in6 ((var address &optional port) &body body)
  `(with-foreign-object (,var 'sockaddr-in6)
     (make-sockaddr-in6 ,var ,address ,port)
     ,@body))

(defun make-sockaddr-un (sun string)
  (declare (type string string))
  (bzero sun size-of-sockaddr-un)
  (with-foreign-slots ((family path) sun sockaddr-un)
    (setf family af-local)
    (with-foreign-string (c-string string)
      (loop :for off :below (1- unix-path-max)
            :do (setf (mem-aref path :uint8 off)
                      (mem-aref c-string :uint8 off)))))
  (values sun))

(defmacro with-sockaddr-un ((var address) &body body)
  `(with-foreign-object (,var 'sockaddr-un)
     (make-sockaddr-un ,var ,address)
     ,@body))

(defmacro with-sockaddr-storage ((var) &body body)
  `(with-foreign-object (,var 'sockaddr-storage)
     (bzero ,var size-of-sockaddr-storage)
     ,@body))

(defmacro with-socklen ((var value) &body body)
  `(with-foreign-object (,var 'socklen)
     (setf (mem-ref ,var 'socklen) ,value)
     ,@body))

(defmacro with-sockaddr-storage-and-socklen ((ss-var size-var) &body body)
  `(with-sockaddr-storage (,ss-var)
     (with-socklen (,size-var size-of-sockaddr-storage)
       ,@body)))

;;;; Misc

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (check-type ,start unsigned-byte "a non-negative integer")
       (check-type ,end (or unsigned-byte null) "a non-negative integer or NIL")
       (unless ,end
         (setq ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(defun %to-octets (buff ef start end)
  (babel:string-to-octets buff :start start :end end
                          :encoding (babel:external-format-encoding ef)))

(declaim (inline ensure-number))
(defun ensure-number (value &key (start 0) end (radix 10) (type t) (errorp t))
  (check-type value (or string unsigned-byte) "a string or an unsigned-byte")
  (let ((parsed
         (etypecase value
           (string
            (ignore-errors (parse-integer value :start start :end end
                                          :radix radix :junk-allowed nil)))
           (t value))))
    (if (and parsed (typep parsed type))
        (values parsed)
        (if errorp
            (error 'parse-error)
            nil))))

(defun ensure-string-or-unsigned-byte (thing &key (type t) (radix 10))
  (or (and (symbolp thing) (string-downcase thing))
      (ensure-number thing :type type :radix radix :errorp nil)
      thing))

(defun lisp->c-bool (val)
  (if val 1 0))

(defun memq (value list)
  (member value list :test #'eq))

(defmacro multiple-value-case ((values &key (test 'eql)) &body body)
  (setf values (ensure-list values))
  (setf test (alexandria::extract-function-name test))
  (assert values () "Must provide at least one value to test")
  (labels ((%do-var (var val)
             (cond
               ((and (symbolp var) (member var '("_" "*") :test #'string=))
                t)
               ((consp var)
                (if (eq 'eq test)
                    `(memq ,val ',var)
                    `(member ,val ',var :test ,test)))
               (t
                `(,test ,val ',var))))
           (%do-clause (c gensyms)
             (destructuring-bind (vals &rest code) c
               (let* ((tests (remove t (mapcar #'%do-var (ensure-list vals) gensyms)))
                      (clause-test (if (> 2 (length tests))
                                       (first tests)
                                       `(and ,@tests))))
                 `(,clause-test ,@code))))
           (%do-last-clause (c gensyms)
             (when c
               (destructuring-bind (test &rest code) c
                 (if (member test '(otherwise t))
                     `((t ,@code))
                     `(,(%do-clause c gensyms)))))))
    (let ((gensyms (mapcar #'(lambda (v) (gensym (string v)))
                           values)))
      `(let ,(mapcar #'list gensyms values)
         (declare (ignorable ,@gensyms))
         (cond ,@(append (mapcar #'(lambda (c) (%do-clause c gensyms))
                                 (butlast body))
                         (%do-last-clause (lastcar body) gensyms)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compute-flags (flags args)
    (loop :with flag-combination := 0
          :for cons :on args :by #'cddr
          :for flag := (car cons)
          :for val := (cadr cons)
          :for const := (cdr (assoc flag flags))
          :when const :do
       (when (not (constantp val)) (return-from compute-flags))
       (setf flag-combination (logior flag-combination const))
       :finally (return flag-combination))))

;;; Reader macros

(defgeneric enable-reader-macro* (name))

(defgeneric disable-reader-macro* (name))

(defmacro enable-reader-macro (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (enable-reader-macro* ,name)))

(defmacro disable-reader-macro (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (disable-reader-macro* ,name)))

(defun save-old-readtable (symbol readtable)
  (setf (getf (symbol-plist symbol) 'old-readtable) readtable))

(defun get-old-readtable (symbol readtable)
  (getf (symbol-plist symbol) 'old-readtable))

(defmethod enable-reader-macro* :before ((name symbol))
  (save-old-readtable name *readtable*)
  (setf *readtable* (copy-readtable)))

(defmethod disable-reader-macro* ((name symbol))
  (assert (readtablep (get-old-readtable name 'old-readtable)))
  (setf *readtable* (get-old-readtable name 'old-readtable))
  (save-old-readtable name nil))

(defmacro define-syntax (name &body body)
  `(defmethod enable-reader-macro* ((name (eql ',name)))
     ,@body))

;;; Literal hash tables reader macro

(defun make-ht-from-list (alist stream test)
  (flet ((err () (error 'reader-error :stream stream))
         (alistp (alist) (every #'consp alist)))
    (unless (alistp alist) (err))
    (alist-hash-table alist :test test :size (length alist))))

(defun read-literal-ht (stream &optional c n)
  (declare (ignore c n))
  (let ((*readtable* (copy-readtable))
        (c (read-char stream))
        (test 'eql))
    (flet ((err () (error 'reader-error :stream stream)))
      (case c
        (#\( t)
        (#\: (let ((l (read-delimited-list #\( stream)))
               (unless (= 1 (length l)) (err))
               (setf test (car l))))
        (t   (err))))
    (make-ht-from-list (read-delimited-list #\) stream)
                       stream test)))

(set-dispatch-macro-character #\# #\h 'read-literal-ht)
