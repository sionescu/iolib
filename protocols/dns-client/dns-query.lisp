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

(defparameter *dns-recursion-desired* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;;  CLASS DEFINITIONS  ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dns-nameserver ()
  ((name     :initarg :name     :accessor dns-nameserver-name)
   (address  :initarg :address  :accessor dns-addressserver-address)
   (sent     :initarg :sent     :accessor dns-sentserver-sent)
   (received :initarg :received :accessor dns-receivedserver-received)))

(defclass dns-message ()
  ((id         :initform 0
               :initarg :id         :accessor dns-message-id)
   (flags      :initform 0
               :initarg :flags      :accessor dns-message-flags)
   (question   :initform nil
               :initarg :question   :accessor dns-message-question)
   (answer     :initform (make-array 1 :adjustable t :fill-pointer 0)
               :initarg :answer     :accessor dns-message-answer)
   (authority  :initform (make-array 1 :adjustable t :fill-pointer 0)
               :initarg :authority  :accessor dns-message-authority)
   (additional :initform (make-array 1 :adjustable t :fill-pointer 0)
               :initarg :additional :accessor dns-message-additional)))

(defmacro define-flags-bitfield (name offset length &optional (type :integer))
  (let ((method-name (et::symbolicate name :-field)))
    `(progn
       (defmethod ,method-name ((message dns-message))
         ,(ecase type
           (:integer `(ldb (byte ,length ,offset) (dns-message-flags message)))
           (:boolean `(logbitp ,offset (dns-message-flags message)))
           (:rcode `(rcode-id
                     (ldb (byte ,length ,offset) (dns-message-flags message))))))
       (defmethod (setf ,method-name) (value (message dns-message))
         ,(ecase type
           (:integer `(setf (ldb (byte ,length ,offset) (dns-message-flags message))
                            value))
           (:boolean `(setf (ldb (byte ,length ,offset) (dns-message-flags message))
                            (lisp->c-bool value)))
           (:rcode `(setf (ldb (byte ,length ,offset) (dns-message-flags message))
                          (rcode-number value))))))))

(define-flags-bitfield response 15 1 :boolean)
(define-flags-bitfield opcode 11 4 :integer)
(define-flags-bitfield authoritative 10 1 :boolean)
(define-flags-bitfield truncated 9 1 :boolean)
(define-flags-bitfield recursion-desired 8 1 :boolean)
(define-flags-bitfield recursion-available 7 1 :boolean)
(define-flags-bitfield rcode 0 4 :rcode)

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

(defmethod initialize-instance :after ((record dns-record) &key)
  (with-slots (name) record
    (let ((name-length (length name)))
      (when (char-not-equal (aref name (1- name-length))
                            #\.)
        (setf name (concatenate 'string name (string #\.)))))))

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


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;;  CONSTRUCTORS  ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-question (qname qtype qclass)
  (make-instance 'dns-question
                 :name qname
                 :type qtype
                 :class qclass))

(defun make-query (id question
                   &optional (recursion-desired *dns-recursion-desired*))
  (let ((msg (make-instance 'dns-message :id id)))
    (setf (opcode-field msg) +opcode-standard+)
    (setf (recursion-desired-field msg) recursion-desired)
    (setf (dns-message-question msg) question)
    msg))


;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;  OUTPUT-RECORD  ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-record ((buffer dynamic-buffer)
                          (record dns-question))
  (with-slots (name type class) record
    (output-domain-name buffer name)
    (output-unsigned-16 buffer (query-type-number type))
    (output-unsigned-16 buffer (query-class-number class))))

(defmethod output-message-header ((buffer dynamic-buffer)
                                  (message dns-message))
  (with-slots (id flags question answer authority additional)
      message
    (output-unsigned-16 buffer id)
    (output-unsigned-16 buffer flags)
    (output-unsigned-16 buffer 1)
    (output-unsigned-16 buffer (length answer))
    (output-unsigned-16 buffer (length authority))
    (output-unsigned-16 buffer (length additional))))

(defmethod output-message ((message dns-message))
  (with-slots (question) message
    (let ((buffer (make-instance 'dynamic-buffer)))
      (output-message-header buffer message)
      (output-record buffer question)
      buffer)))
