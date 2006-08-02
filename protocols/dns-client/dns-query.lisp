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

(defmethod write-dns-string ((buffer dynamic-output-buffer)
                             (string simple-string))
  (write-unsigned-8 buffer (length string))
  (write-vector buffer (sb-ext:string-to-octets string)))

(defun domain-name-to-dns-format (domain-name)
  (let* ((octets (sb-ext:string-to-octets domain-name))
         (tmp-vec (make-array (1+ (length octets))
                              :element-type 'octet)))
    (replace tmp-vec octets :start1 1)
    (let ((vector-length (length tmp-vec)))
      (loop
         :for start-off := 1 then (1+ end-off)
         :for end-off := (or (position (char-code #\.) tmp-vec :start start-off)
                             vector-length)
         :do (setf (aref tmp-vec (1- start-off)) (- end-off start-off))
         :when (>= end-off vector-length) :do (loop-finish)))
    tmp-vec))

(defmethod write-domain-name ((buffer dynamic-output-buffer)
                              (domain-name simple-string))
  (write-vector buffer (domain-name-to-dns-format domain-name)))

(defmethod output-record ((buffer dynamic-output-buffer)
                          (record dns-question))
  (with-slots (name type class) record
    (write-domain-name buffer name)
    (write-unsigned-16 buffer (query-type-number type))
    (write-unsigned-16 buffer (query-class-number class))))

(defmethod output-message-header ((buffer dynamic-output-buffer)
                                  (message dns-message))
  (with-slots (id flags question answer authority additional)
      message
    (write-unsigned-16 buffer id)
    (write-unsigned-16 buffer flags)
    (write-unsigned-16 buffer 1)
    (write-unsigned-16 buffer (length answer))
    (write-unsigned-16 buffer (length authority))
    (write-unsigned-16 buffer (length additional))))

(defmethod output-message ((message dns-message))
  (with-slots (question) message
    (let ((buffer (make-instance 'dynamic-output-buffer)))
      (output-message-header buffer message)
      (output-record buffer question)
      buffer)))
