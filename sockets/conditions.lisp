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

(defmethod error-code ((err system-error))
  (et:system-error-code err))

(defmethod error-identifier ((err system-error))
  (et:system-error-identifier err))

(defmethod error-message ((err system-error))
  (et:message err))

(defun print-message-if-not-null (condition stream &optional (eof-place :before))
  (declare (type stream stream))
  (let ((msg (error-message condition)))
    (when msg
      (when (eql eof-place :before)
        (fresh-line stream))
      (format stream "~A" msg)
      (when (eql eof-place :after)
        (fresh-line stream)))))

(define-condition invalid-argument (system-error)
  ((argument :initarg :argument :reader invalid-argument))
  (:report (lambda (condition stream)
             (declare (type stream stream))
             (format stream "Invalid argument: ~A"
                     (invalid-argument condition))
             (print-message-if-not-null condition stream)))
  (:documentation "Signaled when one or more of a function's arguments are considered
not valid(wrong type or good type but not within a certain range of
values, et caetera)."))

;;;;;;;;;;;;;;;;;;;;;;;
;;;  SOCKET ERRORS  ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *socket-error-map* nil)

(define-condition socket-error (unix-error) ())

(defmacro define-socket-error (name identifier &optional documentation)
  `(progn
     (export ',name)
     (push (cons ,identifier ',name) *socket-error-map*)
     (define-condition ,name (socket-error) ()
       (:default-initargs :code (unixerr-value ,identifier)
                          :identifier ,identifier)
       (:documentation ,documentation))))

(define-socket-error socket-address-in-use-error          :eaddrinuse)
(define-socket-error socket-address-not-available-error   :eaddrnotavail)
(define-socket-error socket-network-down-error            :enetdown)
(define-socket-error socket-network-reset-error           :enetreset)
(define-socket-error socket-network-unreachable-error     :enetunreach)
(define-socket-error socket-no-network-error              :enonet)
(define-socket-error socket-connection-aborted-error      :econnaborted)
(define-socket-error socket-connection-reset-error        :econnreset)
(define-socket-error socket-connection-refused-error      :econnrefused)
(define-socket-error socket-endpoint-shutdown-error       :eshutdown)
(define-socket-error socket-connection-timeout-error      :etimedout)
(define-socket-error socket-no-buffer-space-error         :enobufs)
(define-socket-error socket-host-down-error               :ehostdown)
(define-socket-error socket-host-unreachable-error        :ehostunreach)
(define-socket-error socket-already-connected-error       :eisconn)
(define-socket-error socket-not-connected-error           :enotconn)
(define-socket-error socket-option-not-supported-error    :enoprotoopt)
(define-socket-error socket-operation-not-supported-error :eopnotsupp)

(defun socket-error (unix-error)
  (let* ((id (error-identifier unix-error))
         (condition (cdr (assoc id *socket-error-map*))))
    (if condition
        (error condition)
        (error unix-error))))

(defmacro with-socket-error-filter (&body body)
  `(handler-case
       (progn ,@body)
     (unix-error (err)
       (socket-error err))))
