;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; conditions.lisp --- Conditions.
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

(defgeneric error-code (err))

(defmethod error-code ((err system-error))
  (nix:system-error-code err))

(defgeneric error-identifier (err))

(defmethod error-identifier ((err system-error))
  (nix:system-error-identifier err))

(defgeneric error-message (err))

(defmethod error-message ((err system-error))
  (nix:system-error-message err))

(defun print-message-if-not-null (condition stream
                                  &optional (eof-place :before))
  (declare (type stream stream))
  (let ((msg (error-message condition)))
    (when msg
      (when (eql eof-place :before)
        (fresh-line stream))
      (format stream "~A" msg)
      (when (eql eof-place :after)
        (fresh-line stream)))))

;;;; Socket Errors

(defvar *socket-error-map* nil)

(define-condition socket-error (posix-error) ())

(defmacro define-socket-error (name identifier &optional documentation)
  `(progn
     (push (cons ,identifier ',name) *socket-error-map*)
     (define-condition ,name (socket-error) ()
       (:default-initargs :code (unixerr-value ,identifier)
                          :identifier ,identifier)
       (:documentation ,(or documentation "")))))

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

(defun socket-error (posix-error)
  (let* ((id (error-identifier posix-error))
         (condition (cdr (assoc id *socket-error-map*))))
    (if condition
        (error condition)
        (error posix-error))))

(defmacro with-socket-error-filter (&body body)
  `(handler-case
       (progn ,@body)
     (posix-error (err)
       (socket-error err))))
