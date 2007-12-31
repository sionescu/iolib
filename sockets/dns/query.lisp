;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; query.lisp --- Make DNS queries.
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

(defvar *dns-recursion-desired* t
  "Whether the \"RECURSION-DESIRED\" field should be set ot not.")

(defvar *dns-repeat* 5
  "The number of times a failed query will be retried.")

(defvar *dns-timeout* 5
  "Timeout for DNS queries in seconds.")

(defconstant +dns-port+ 53)

(defun send-query (socket-type buffer nameserver timeout)
  (let ((input-buffer (make-array +dns-datagram-size+
                                  :element-type 'ub8)))
    (with-open-socket
        (socket :connect :active :type socket-type
                :remote-host nameserver :remote-port +dns-port+
                :ipv6 (ipv6-address-p nameserver))
      (socket-send buffer socket)
      (iomux:wait-until-fd-ready (fd-of socket) :read timeout)
      (socket-receive input-buffer socket))))

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
    (loop :for target-index :below vector-length
          :for source-index := (- vector-length target-index 1)
          :do (setf (aref reverse-vector target-index)
                    (aref vector source-index)))
    (values reverse-vector)))

(defun ipv4-dns-ptr-name (address)
  (declare (type ipv4-array address))
  (concatenate 'string (vector-to-dotted (reverse-vector address))
               ".in-addr.arpa."))

(defun ipv6-vector-to-dotted (vector)
  (declare (type ipv6-array vector))
  (with-standard-io-syntax
    (let ((*print-base* 16))
      (with-output-to-string (dotted-address)
        (loop :for index :below (length vector)
              :for element := (aref vector index) :do
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

(defmethod %decode-rr ((rr dns-rr) type class)
  (declare (ignore type class))
  (cons (dns-rr-ttl rr) (dns-rr-data rr)))

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

(defun decode-rr (rr)
  (%decode-rr rr (dns-record-type rr) (dns-record-class rr)))

;;;; Response Decoding

(defgeneric %decode-response (dns-message question-type))

(defmethod %decode-response :around ((msg dns-message) question-type)
  (declare (ignore question-type))
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
            (loop :for i :from (1+ first-address-place)
                  :below (dns-message-answer-count msg)
                  :collect (decode-rr (aref answer i)))))
    (values cname first-address other-addresses)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :a)))
  (declare (ignore question-type))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :aaaa)))
  (declare (ignore question-type))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :ptr)))
  (declare (ignore question-type))
  (decode-rr (aref (dns-message-answer msg) 0)))

;; TODO: got a lot to do here
(defmethod %decode-response ((msg dns-message) (question-type (eql :mx)))
  (declare (ignore question-type))
  (let ((rr (aref (dns-message-answer msg) 0)))
    (decode-rr rr)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :txt)))
  (declare (ignore question-type))
  (decode-rr (aref (dns-message-answer msg) 0)))

(defmethod %decode-response ((msg dns-message) question-type)
  (declare (ignore question-type))
  (values msg))

(defun decode-response (message)
  (%decode-response message
                    (dns-record-type
                     (aref (dns-message-question message) 0))))

;;;; DNS-QUERY

(defun dns-query (name &key (type :a) nameserver decode search
                  (repeat *dns-repeat*) (timeout *dns-timeout*))
  ;; TODO: implement search
  (declare (ignore search))
  (bt:with-lock-held (*resolvconf-lock*)
    (unless nameserver (setf nameserver *dns-nameservers*)))
  (when (eq type :ptr)
    (setf name (dns-ptr-name name)))
  (let* ((query (prepare-query name type))
         (buffer (sequence-of query))
         (bufflen (length buffer))
         (tries-left repeat)
         in-buff bytes-received response tcp-done)
    ;; at the moment only one nameserver is used
    (when (listp nameserver)
      (setf nameserver (car nameserver)))
    (assert nameserver (nameserver) "Must supply a nameserver")
    (tagbody
     :start
       (setf tcp-done nil
             response nil)
       ;; if the query size fits into a datagram(512 bytes max) do a
       ;; UDP query, otherwise use TCP
       (when (> bufflen +dns-datagram-size+)
         (go :do-tcp-query))
     :do-udp-query
       ;; do a UDP query; in case of a socket error, try again
       (handler-case
           (setf (values in-buff bytes-received)
                 (send-query :datagram buffer nameserver timeout))
         (socket-error ()
           (go :try-again-if-possible)))
       ;; no socket error, go parse the response
       (go :parse-response)
     :do-tcp-query
       ;; do a TCP query; in case of a socket error, try again
       (handler-case
           (setf (values in-buff bytes-received)
                 (send-query :stream buffer nameserver timeout))
         (socket-error ()
           (go :try-again-if-possible)))
       (setf tcp-done t)
     :parse-response
       ;; try to parse the response; in case of a parse error, try again
       (handler-case
           (setf response
                 (read-dns-message
                  (make-instance 'dynamic-buffer
                                 :sequence in-buff
                                 :size bytes-received)))
         (dynamic-buffer-input-error ()
           (go :try-again-if-possible))
         (dns-message-error ()
           (go :try-again-if-possible)))
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
