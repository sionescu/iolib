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

(in-package :net.sockets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;;  CLASS DEFINITIONS  ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((method-name (iolib-utils:concat-symbol name :-field)))
    `(progn
       (defgeneric ,method-name (message))
       (defmethod ,method-name ((message dns-message))
         ,(ecase type
           (:integer `(ldb (byte ,length ,offset) (dns-message-flags message)))
           (:boolean `(logbitp ,offset (dns-message-flags message)))
           (:rcode `(rcode-id
                     (ldb (byte ,length ,offset) (dns-message-flags message))))))
       (defgeneric (setf ,method-name) (value message))
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
                                       qdcount ancount nscount arcount)
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

(defun make-query (id question &optional recursion-desired)
  (let ((msg (make-instance 'dns-message :id id)))
    (setf (opcode-field msg) +opcode-standard+)
    (setf (recursion-desired-field msg) recursion-desired)
    (vector-push-extend question (dns-message-question msg))
    msg))


;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;  OUTPUT-RECORD  ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric write-dns-string (buffer string))
(defmethod write-dns-string ((buffer dynamic-output-buffer)
                             (string string))
  (write-unsigned-8 buffer (length string))
  (write-vector buffer (flexi-streams:string-to-octets string)))

(defun domain-name-to-dns-format (domain-name)
  (let* ((octets (flexi-streams:string-to-octets domain-name))
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
