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
(defparameter *dns-nameservers* nil)
(defparameter *dns-repeat* 5)
(defparameter *dns-timeout* 5)

(defun do-udp-query (buffer nameserver timeout)
  (let ((socket (make-socket :type :datagram))
        (input-buffer (make-array +dns-datagram-size+
                                  :element-type 'octet)))
    (unwind-protect
         (progn
           (connect socket nameserver :port 53)
           (socket-send buffer socket)
           (socket-receive input-buffer socket))
      (socket-close socket))))

(defun do-tcp-query (buffer nameserver timeout)
  (let ((socket (make-socket :type :stream))
        (input-buffer (make-array +dns-datagram-size+
                                  :element-type 'octet)))
    (unwind-protect
         (progn
           (connect socket nameserver :port 53)
           (socket-send buffer socket)
           (socket-receive input-buffer socket))
      (socket-close socket))))

(define-constant +max-16-bits+ (1- (expt 2 16)))

(defun prepare-query (name type)
  (let* ((question (make-question name type :in))
         (query (make-query (random +max-16-bits+)
                            question *dns-recursion-desired*)))
    (write-dns-message query)))

(defun dns-query (name &key (type :a) (nameserver *dns-nameservers*)
                  (repeat *dns-repeat*) (timeout *dns-timeout*)
                  (decode nil) (search nil))
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
       (multiple-value-setq (in-buff bytes-received)
         (if (> bufflen +dns-datagram-size+)
             (go :do-tcp-query)
             (handler-case
                 (do-udp-query buffer nameserver timeout)
               (socket-error (err)
                 (declare (ignore err))
                 (go :try-again-if-possible)))))
       ;; if no socket error, go parse the response
       (go :parse-response)

     :do-tcp-query
       ;; do a TCP query; in case of a socket error, try again
       (multiple-value-setq (in-buff bytes-received)
         (handler-case
             (do-tcp-query buffer nameserver timeout)
           (socket-error (err)
             (declare (ignore err))
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
               (input-buffer-error (err)
                 (declare (ignore err))
                 (go :try-again-if-possible))
               (dns-message-error (err)
                 (declare (ignore err))
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
         (return-from dns-query response))

     :raise-error
       (error "Could not query nameserver !!"))))
