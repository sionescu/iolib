;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
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

;;;; Conversion functions for SOCKADDR_* structs

(defun sockaddr-in->sockaddr (sin)
  (with-foreign-slots ((addr port) sin sockaddr-in)
    (values (make-instance 'ipv4-address
                           :name (integer-to-vector (ntohl addr)))
            (ntohs port))))

(defun sockaddr-in6->sockaddr (sin6)
  (with-foreign-slots ((addr port) sin6 sockaddr-in6)
    (values (make-instance 'ipv6-address
                           :name (in6-addr-to-ipv6-array addr))
            (ntohs port))))

(defun sockaddr-un->sockaddr (sun)
  (with-foreign-slots ((path) sun sockaddr-un)
    (let ((name (make-string (1- unix-path-max)))
          (abstract nil))
      (cond ((zerop (mem-aref path :uint8 0))
             ;; abstract address
             (setf abstract t)
             (loop :for sindex :from 0 :below (1- unix-path-max)
                   :for pindex :from 1 :below unix-path-max
                   :do (setf (schar name sindex)
                             (code-char (mem-aref path :uint8 pindex)))))
            (t 
             ;; address is in the filesystem
             (setf name (foreign-string-to-lisp path))))
      (make-instance 'local-address
                     :name name
                     :abstract abstract))))

(defun sockaddr-storage->sockaddr (ss)
  (with-foreign-slots ((family) ss sockaddr-storage)
    (ecase family
      (#.af-inet (sockaddr-in->sockaddr ss))
      (#.af-inet6 (sockaddr-in6->sockaddr ss))
      (#.af-local (sockaddr-un->sockaddr ss)))))

(defun sockaddr->sockaddr-storage (ss sockaddr &optional (port 0))
  (etypecase sockaddr
    (ipv4-address (make-sockaddr-in ss (address-name sockaddr) port))
    (ipv6-address (make-sockaddr-in6 ss (address-name sockaddr) port))
    (local-address (make-sockaddr-un ss (address-name sockaddr)))))

;;;; Misc

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (unless ,end
         (setq ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(defun %to-octets (buff ef start end)
  (babel:string-to-octets buff :start start :end end
                          :encoding (babel:external-format-encoding ef)))

(defun parse-number-or-nil (value &optional (type :any) (radix 10))
  (check-type value (or string unsigned-byte) "a string or an unsigned-byte")
  (let ((parsed
         (if (stringp value)
             (ignore-errors (parse-integer value :radix radix
                                           :junk-allowed nil))
             value)))
    (and parsed
         ;; if it's a number and its type is ok return it
         (typep parsed (ecase type
                         (:any  t)     (:ub8  'ub8)
                         (:ub16 'ub16) (:ub32 'ub32)))
         (values parsed))))

(defun ensure-string-or-unsigned-byte (thing &optional (type :any) (radix 10))
  (or (and (symbolp thing) (string-downcase thing))
      (parse-number-or-nil thing type radix)
      thing))

(defun lisp->c-bool (val)
  (if val 1 0))

(defmacro with-socklen ((var value) &body body)
  `(with-foreign-object (,var 'socklen)
     (setf (mem-ref ,var 'socklen) ,value)
     ,@body))

(defun memq (value list)
  (member value list :test #'eq))

(defmacro multiple-value-case (values &body body)
  (setf values (ensure-list values))
  (assert values () "Must provide at least one value to test")
  (labels ((%do-var (var val)
             (cond
               ((and (symbolp var) (string= var "_"))
                t)
               ((consp var)
                `(memq ,val ',var))
               (t
                `(eq ,val ',var))))
           (%do-clause (c)
             (destructuring-bind ((&rest vals) &rest code) c
               `((and ,@(mapcar #'%do-var vals values)) ,@code)))
           (%do-last-clause (c)
             (when c
               (destructuring-bind (test &rest code) c
                 (if (member test '(otherwise t))
                     `((t ,@code))
                     `(,(%do-clause c)))))))
    `(cond ,@(append (mapcar #'%do-clause (butlast body))
                     (%do-last-clause (car (last body)))))))

;;; Reader macros

(defun make-ht-from-list (list stream test)
  (flet ((err () (error 'reader-error :stream stream))
         (separatorp (s) (eq s '|,|))
         (middlep (s) (eq s '=>)))
    (let* ((ht (make-hash-table :test test))
           (series (scan 'list list))
           (separators (choose (mask (scan-range :from 3 :by 4)) series)))        
      (unless (collect-and (#mseparatorp separators)) (err))
      (multiple-value-bind (keys middles values)
          (chunk 3 4 series)
        (unless (collect-and (#mmiddlep middles)) (err))
        (collect-hash keys values :test test)))))

(defun read-literal-ht (stream &optional c n)
  (declare (ignore c n))
  (let ((*readtable* (copy-readtable))
        (c (read-char stream))
        (list '())
        (test 'eql))
    (set-syntax-from-char #\} #\))
    (set-syntax-from-char #\{ #\))
    (set-syntax-from-char #\, #\$)
    (flet ((err () (error 'reader-error :stream stream)))
      (case c
        (#\{
         (setf list (read-delimited-list #\} stream)))
        (#\:
         (let ((l (read-delimited-list #\{ stream)))
           (unless (= 1 (length l)) (err))
           (setf test (car l))
           (setf list (read-delimited-list #\} stream))))
        (t
         (err))))
    (values (make-ht-from-list list stream test))))

(set-dispatch-macro-character #\# #\h 'read-literal-ht)
