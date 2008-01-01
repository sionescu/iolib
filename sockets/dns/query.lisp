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

(defvar *dns-repeat* 3
  "The number of times a failed query will be retried.")

(defvar *dns-timeout* 10
  "Timeout for DNS queries in seconds.")

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

(defconstant +dns-port+ 53)

(defun do-udp-dns-query (buffer length nameserver timeout)
  (let ((input-buffer (make-array +dns-datagram-size+
                                  :element-type 'ub8)))
    (with-open-stream
        (socket (make-socket :connect :active :type :datagram
                             :remote-host nameserver :remote-port +dns-port+
                             :ipv6 (ipv6-address-p nameserver)))
      (socket-send buffer socket :end length)
      (iomux:wait-until-fd-ready (fd-of socket) :read timeout t)
      (socket-receive input-buffer socket))))

(defun wait-until-socket-connected (socket timeout)
  (if (nth-value 1 (iomux:wait-until-fd-ready (fd-of socket) :write timeout))
      (let ((errcode (get-socket-option socket :error)))
        (when (minusp errcode) (signal-socket-error)))
      (error 'socket-connection-timeout-error)))

(defun send-tcp-dns-query (socket buffer length)
  (let ((minibuf (make-array (+ length 2) :element-type 'ub8)))
    ;; two-octet length prefix
    (replace minibuf (ub16-to-vector length))
    (replace minibuf buffer :start1 2 :end2 length)
    (socket-send minibuf socket :end (+ length 2))))

(defun get-tcp-query-length (socket timeout)
  (let ((minibuf (make-array 2 :element-type 'ub8)))
    (iomux:wait-until-fd-ready (fd-of socket) :read timeout t)
    (socket-receive minibuf socket)
    (+ (ash (aref minibuf 0) 8)
       (aref minibuf 1))))

(defun receive-tcp-dns-message (socket time-fn)
  (with-accessors ((fd fd-of)) socket
    (let* ((message-length (get-tcp-query-length socket (funcall time-fn)))
           (input-buffer (make-array message-length :element-type 'ub8)))
      (loop :with off := 0 :do
         (iomux:wait-until-fd-ready fd :read (funcall time-fn) t)
         (let ((inbytes (nth-value 1 (socket-receive input-buffer socket :start off))))
           (incf off inbytes)
           (when (= off message-length)
             (return (values input-buffer off))))))))

(defun do-tcp-dns-query (buffer length nameserver timeout)
  (let* ((t0 (osicat-sys:get-monotonic-time))
         (tend (+ t0 timeout)))
    (flet ((remtime ()
             (let ((rem (- tend (osicat-sys:get-monotonic-time))))
               (if (not (minusp rem)) rem
                   (error 'socket-connection-timeout-error)))))
      (with-open-stream
          (socket (make-socket :connect :active :type :stream
                               :ipv6 (ipv6-address-p nameserver)))
        (setf (fd-non-blocking socket) t)
        (handler-case
            (connect socket nameserver :port +dns-port+)
          (socket-connection-in-progress-error ()
            (wait-until-socket-connected socket (remtime))))
        (send-tcp-dns-query socket buffer length)
        (receive-tcp-dns-message socket #'remtime)))))

(defun do-one-dns-query (name type search decode ns repeat timeout)
  ;; TODO: implement search
  (declare (ignore search))
  (let* ((query (prepare-query name type))
         (buffer (sequence-of query))
         (bufflen (write-cursor-of query))
         (tries-left repeat)
         in-buff bytes-received response tcp-done)
    (tagbody
     :start
       (setf tcp-done nil response nil)
       ;; if the query size fits into a datagram(512 bytes max) do a
       ;; UDP query, otherwise use TCP
       (when (> bufflen +dns-datagram-size+)
         (go :do-tcp-query))
     :do-udp-query
       ;; do a UDP query; in case of a socket error, try again
       (handler-case
           (setf (values in-buff bytes-received)
                 (do-udp-dns-query buffer bufflen ns timeout))
         (socket-error () (go :error))
         (iomux:poll-timeout () (go :try-again-if-possible)))
       ;; no socket error, go parse the response
       (go :parse-response)
     :do-tcp-query
       ;; do a TCP query; in case of a socket error, try again
       (handler-case
           (setf (values in-buff bytes-received)
                 (do-tcp-dns-query buffer bufflen ns timeout))
         (socket-connection-timeout-error () (go :try-again-if-possible))
         (socket-error () (go :error))
         (iomux:poll-timeout () (go :try-again-if-possible)))
       (setf tcp-done t)
     :parse-response
       ;; try to parse the response; in case of a parse error, try again
       (handler-case
           (setf response
                 (read-dns-message
                  (make-instance 'dynamic-buffer
                                 :sequence in-buff
                                 :size bytes-received)))
         (dynamic-buffer-input-error () (go :error))
         (dns-message-error () (go :error)))
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
         (return-from do-one-dns-query
           (if decode
               (decode-response response)
               response)))
     :error
       (return-from do-one-dns-query))))

(defun preprocess-dns-name (name type)
  (if (eq type :ptr)
      (dns-ptr-name name)
      name))

(defun dns-query (name &key (type :a) decode search
                  (nameservers *dns-nameservers*)
                  (repeat *dns-repeat*) (timeout *dns-timeout*))
  (setf nameservers (ensure-list nameservers))
  (assert nameservers (nameservers) "Must supply a nameserver")
  (let ((pname (preprocess-dns-name name type)))
    (dolist (ns (mapcar #'ensure-address nameservers))
      (when-let ((response (do-one-dns-query pname type search decode
                                             ns repeat timeout)))
        (return-from dns-query response)))))
