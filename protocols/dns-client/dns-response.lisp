;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package #:net.sockets)

(defclass dns-rr (dns-record)
  ((ttl  :initarg :ttl  :accessor dns-rr-ttl)))

(defmethod initialize-instance :after ((rr dns-rr) &key)
  (with-slots (ttl) rr
    (check-type ttl (unsigned-byte 32) "a valid TTL")))

(defclass dns-rr-a (dns-rr)
  ((address :initarg :address :accessor dns-rr-a-address))
  (:default-initargs :type :a))

(defclass dns-rr-ns (dns-rr)
  ((dname :initarg :dname :accessor dns-rr-ns-dname))
  (:default-initargs :type :ns))

(defclass dns-rr-cname (dns-rr) ()
  (:default-initargs :type :cname))

(defclass dns-rr-soa (dns-rr)
  ((mname   :initarg :mname   :accessor dns-rr-soa-mname)
   (rname   :initarg :rname   :accessor dns-rr-soa-rname)
   (serial  :initarg :serial  :accessor dns-rr-soa-serial)
   (refresh :initarg :refresh :accessor dns-rr-soa-refresh)
   (retry   :initarg :retry   :accessor dns-rr-soa-retry)
   (expire  :initarg :expire  :accessor dns-rr-soa-expire)
   (minimum :initarg :minimum :accessor dns-rr-soa-minimum))
  (:default-initargs :type :soa))

(defclass dns-rr-wks (dns-rr)
  ((address :initarg :address :accessor dns-rr-wks-address)
   (protocol :initarg :protocol :accessor dns-rr-wks-protocol)
   (bitmap :initarg :bitmap :accessor dns-rr-wkx-bitmap))
  (:default-initargs :type :wks))

(defclass dns-rr-ptr (dns-rr)
  ((dname :initarg :dname :accessor dns-rr-ptr-dname))
  (:default-initargs :type :ptr))

(defclass dns-rr-hinfo (dns-rr)
  ((cpu :initarg :cpu :accessor dns-rr-hinfo-cpu)
   (os  :initarg :os  :accessor dns-rr-hinfo-os))
  (:default-initargs :type :hinfo))

(defclass dns-rr-mx (dns-rr)
  ((preference :initarg :preference :accessor dns-rr-mx-preference)
   (exchange   :initarg :exchange   :accessor dns-rr-mx-exchange))
  (:default-initargs :type :mx))

(defclass dns-rr-txt (dns-rr)
  ((data :initarg :data :accessor dns-rr-txt-data))
  (:default-initargs :type :txt))

(defclass dns-rr-aaaa (dns-rr)
  ((address :initarg :address :accessor dns-rr-aaaa-address))
  (:default-initargs :type :aaaa))

(defmethod add-answer-rr ((message dns-message)
                          (record dns-rr))
  (vector-push-extend record (dns-message-answer message)))

(defmethod add-authority-rr ((message dns-message)
                             (record dns-rr))
  (vector-push-extend record (dns-message-authority message)))

(defmethod add-additional-rr ((message dns-message)
                              (record dns-rr))
  (vector-push-extend record (dns-message-additional message)))


(define-condition dns-message-error (error) ()
  (:documentation "Signaled when a format error is encountered while parsing a DNS message"))

(defmethod read-dns-string ((buffer dynamic-input-buffer))
  (let ((length (read-unsigned-8 buffer)))
    (sb-ext:octets-to-string (read-vector buffer length))))

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
               (reduce #'(lambda (str1 str2)
                           (concatenate 'string str1 connector str2))
                       (cdr strings)
                       :initial-value "")))

(defmethod dns-domain-name-to-string ((buffer dynamic-input-buffer))
  (let (string offset pointer-seen)
    (values
     (join "." (loop
                  :for (pointer . rec) := (read-dns-pointer-recursively
                                           (buffer-sequence buffer)
                                           (buffer-position buffer))
                  :do (progn
                        (when (not pointer-seen)
                          (if rec
                              (progn
                                (setf pointer-seen t)
                                (setf offset (+ (buffer-position buffer) 2)))
                              (setf offset (+ (buffer-position buffer) 1))))
                        (format t "~s ~a ~a ~a ~a~%" string offset pointer-seen pointer rec)
                        (buffer-seek buffer pointer)
                        (setf string (read-dns-string buffer)))
                  :collect string
                  :until (string= string "")))
     offset)))

(defmethod read-domain-name ((buffer dynamic-input-buffer))
  (with-slots (sequence position) buffer
    (multiple-value-bind (string offset)
        (dns-domain-name-to-string buffer)
      (setf position offset)
      string)))
