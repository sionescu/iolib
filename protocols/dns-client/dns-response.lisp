;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
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

(in-package :net.sockets)

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
  (:documentation "Signaled when a format error is encountered while parsing a DNS message"))

(defgeneric read-dns-string (buffer))
(defmethod read-dns-string ((buffer dynamic-input-buffer))
  (let ((length (read-unsigned-8 buffer)))
    (io.encodings:octets-to-string (read-vector buffer length))))

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

(defgeneric dns-domain-name-to-string (buffer))
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
                        (buffer-seek buffer pointer)
                        (setf string (read-dns-string buffer)))
                  :collect string
                  :until (string= string "")))
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
    (loop
       :for i :below 4
       :do (setf (aref address i) (read-unsigned-8 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :aaaa)) (class (eql :in))
                         resource-length)
  (unless (= resource-length 16)
    (error 'dns-message-error))
  (let ((address (make-array 8 :element-type '(unsigned-byte 16))))
    (loop
       :for i :below 8
       :do (setf (aref address i) (read-unsigned-16 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :cname)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer)) ; CNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :hinfo)) (class (eql :in))
                         resource-length)
  (list (read-dns-string buffer)   ; CPU
        (read-dns-string buffer))) ; OS

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :mx)) (class (eql :in))
                         resource-length)
  (list (read-unsigned-16 buffer)   ; PREFERENCE
        (read-domain-name buffer))) ; EXCHANGE

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :ns)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer)) ; NSDNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :ptr)) (class (eql :in))
                         resource-length)
  (read-domain-name buffer)) ; PTRDNAME

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :soa)) (class (eql :in))
                         resource-length)
  (list (read-domain-name buffer)   ; MNAME
        (read-domain-name buffer)   ; RNAME
        (read-unsigned-32 buffer)   ; SERIAL
        (read-unsigned-32 buffer)   ; REFRESH
        (read-unsigned-32 buffer)   ; RETRY
        (read-unsigned-32 buffer)   ; EXPIRE
        (read-unsigned-32 buffer))) ; MINIMUM

(defmethod read-rr-data ((buffer dynamic-input-buffer)
                         (type (eql :txt)) (class (eql :in))
                         resource-length)
  (loop
     :for string := (read-dns-string buffer) ; TXT-DATA
     :for total-length := (1+ (length string)) :then (+ total-length 1
                                                        (length string))
     :collect string
     :until (>= total-length resource-length)
     :finally (when (> total-length resource-length)
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
      (loop
         :for i :below (dns-message-question-count msg)
         :for q := (read-question buffer)
         :do (add-question msg q))
      (loop
         :for i :below (dns-message-answer-count msg)
         :for rr := (read-dns-rr buffer)
         :do (add-answer-rr msg rr))
      (loop
         :for i :below (dns-message-authority-count msg)
         :for rr := (read-dns-rr buffer)
         :do (add-authority-rr msg rr))
      (loop
         :for i :below (dns-message-additional-count msg)
         :for rr := (read-dns-rr buffer)
         :do (add-additional-rr msg rr)))
    msg))
