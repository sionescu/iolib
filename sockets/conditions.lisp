;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; conditions.lisp --- Socket conditions.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(defgeneric error-code (err)
  (:method ((err system-error))
    (osicat-sys:system-error-code err)))

(defgeneric error-identifier (err)
  (:method ((err system-error))
    (osicat-sys:system-error-identifier err)))

(defgeneric error-message (err)
  (:method ((err system-error))
    (osicat-sys:system-error-message err)))

(defun print-message-if-not-null (condition stream &optional
                                  (eof-place :before))
  (declare (type stream stream))
  (let ((msg (error-message condition)))
    (when msg
      (when (eq eof-place :before)
        (fresh-line stream))
      (format stream "~A" msg)
      (when (eq eof-place :after)
        (fresh-line stream)))))

;;;; Socket Errors

(define-condition socket-error (nix:posix-error) ())

(defmethod print-object ((socket-error socket-error) stream)
  (print-unreadable-object (socket-error stream :type t :identity nil)
    (let ((code (osicat-sys:system-error-code socket-error)))
      (format stream "~S ~S ~S"
              (or code "[Unknown code]")
              (error-identifier socket-error)
              (if code (nix:strerror code)
                  "[Can't get error string.]")))))

(defvar *socket-error-map* (make-hash-table :test 'eq))

(defmacro define-socket-error (name identifier &optional documentation)
  `(progn
     (setf (gethash ,identifier *socket-error-map*) ',name)
     (define-condition ,name (,(nix::get-posix-error-condition identifier)
                              socket-error) ()
       (:default-initargs :code ,(foreign-enum-value 'socket-error-values
                                                     identifier)
         :identifier ,identifier)
       (:documentation ,(or documentation (nix:strerror identifier))))))

(defun lookup-socket-error (keyword)
  (gethash keyword *socket-error-map*))

(define-condition unknown-socket-error (socket-error) ()
  (:documentation "Error signalled upon finding an unknown socket error."))

(define-socket-error socket-invalid-argument              :einval)
(define-socket-error socket-address-in-use-error          :eaddrinuse)
(define-socket-error socket-address-not-available-error   :eaddrnotavail)
(define-socket-error socket-network-down-error            :enetdown)
(define-socket-error socket-network-reset-error           :enetreset)
(define-socket-error socket-network-unreachable-error     :enetunreach)
(define-socket-error socket-no-network-error              :enonet)
(define-socket-error socket-connection-aborted-error      :econnaborted)
(define-socket-error socket-connection-reset-error        :econnreset)
(define-socket-error socket-connection-refused-error      :econnrefused)
(define-socket-error socket-connection-timeout-error      :etimedout)
(define-socket-error socket-connection-in-progress-error  :einprogress)
(define-socket-error socket-endpoint-shutdown-error       :eshutdown)
(define-socket-error socket-no-buffer-space-error         :enobufs)
(define-socket-error socket-host-down-error               :ehostdown)
(define-socket-error socket-host-unreachable-error        :ehostunreach)
(define-socket-error socket-already-connected-error       :eisconn)
(define-socket-error socket-not-connected-error           :enotconn)
(define-socket-error socket-option-not-supported-error    :enoprotoopt)
(define-socket-error socket-operation-not-supported-error :eopnotsupp)

(defun %socket-error (id code)
  (when-let ((err (lookup-socket-error id)))
    (error err :identifier id :code code)))

;;; Used in the ERRNO-WRAPPER foreign type.
(defun signal-socket-error (&optional return-value)
  (declare (ignore return-value))
  (let* ((errno (nix:get-errno))
         (kw (foreign-enum-keyword 'socket-error-values
                                   errno :errorp nil)))
    (or (%socket-error kw errno)
        (error (nix::make-posix-error errno)))))
