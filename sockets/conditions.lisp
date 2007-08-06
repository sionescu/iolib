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

;;; we need these early on, can't wait for winsock.lisp.
#+windows
(progn
  (load-foreign-library "Ws2_32.dll")
  (load-foreign-library "msvcrt.dll"))

(defgeneric error-code (err))

(defmethod error-code ((err system-error))
  (osicat-sys:system-error-code err))

(defgeneric error-identifier (err))

(defmethod error-identifier ((err system-error))
  (osicat-sys:system-error-identifier err))

(defgeneric error-message (err))

(defmethod error-message ((err system-error))
  (osicat-sys:system-error-message err))

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

;;; Should we really be subclassing POSIX-ERROR even on windows? Maybe.
(define-condition socket-error (nix:posix-error) ())

(defmethod print-object ((socket-error socket-error) stream)
  (print-unreadable-object (socket-error stream :type t :identity nil)
    (let ((code (osicat-sys:system-error-code socket-error)))
      (format stream "~S ~S ~S"
              (or code "[No code]")
              (osicat-sys:system-error-identifier socket-error)
              (or #-windows (nix:strerror code)
                  #+windows (get-wsa-error-string code)
                  "[Can't get error string.]")))))

(defmacro define-socket-error (name identifier &optional documentation)
  `(progn
     (push (cons ,identifier ',name) *socket-error-map*)
     (define-condition ,name (socket-error) ()
       (:default-initargs :code ,(foreign-enum-value 'socket-error-values
                                                     identifier)
         :identifier ,identifier)
       (:documentation ,(or documentation "")))))

(define-condition unknown-socket-error (socket-error)
  ()
  (:documentation "Error signalled upon finding an unknown error."))

(defun lookup-socket-error (keyword)
  (or (cdr (assoc keyword *socket-error-map*))
      (make-instance 'unknown-socket-error :identifier keyword
                     :code (foreign-enum-value 'socket-error-values keyword))))

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
(define-socket-error socket-endpoint-shutdown-error       :eshutdown)
(define-socket-error socket-connection-timeout-error      :etimedout)
(define-socket-error socket-no-buffer-space-error         :enobufs)
(define-socket-error socket-host-down-error               :ehostdown)
(define-socket-error socket-host-unreachable-error        :ehostunreach)
(define-socket-error socket-already-connected-error       :eisconn)
(define-socket-error socket-not-connected-error           :enotconn)
(define-socket-error socket-option-not-supported-error    :enoprotoopt)
(define-socket-error socket-operation-not-supported-error :eopnotsupp)

;;; For regular POSIX functions that return extra errors when handling
;;; sockets.  Eg.: CLOSE and OPEN.  But maybe we should simply define
;;; our own bindings for those functions at some point in order to
;;; ditch the CL-POSIX dependency? (especially if we at some point use
;;; HANDLEs instead of FDs on windows)
(defmacro with-socket-error-filter (&body body)
  `(handler-case
       (progn ,@body)
     (nix:posix-error (err)
       (let* ((id (error-identifier err))
              (condition (cdr (assoc id *socket-error-map*))))
         (if condition
             (error condition)
             (error err))))))

#+windows
(defcfun ("WSAGetLastError" wsa-get-last-error :cconv :stdcall) :int)

;;; Used in the ERRNO-WRAPPER foreign type.
(defun signal-socket-error (return-value)
  (declare (ignore return-value))
  (let ((errno #-windows (nix:get-errno)
               #+windows (wsa-get-last-error)))
    (let ((kw (foreign-enum-keyword 'socket-error-values errno :errorp nil)))
      (if kw
          (error (lookup-socket-error kw))
          ;; this branch is probably mostly unused now. Should
          ;; probably sinal an UNKOWN-SOCKET-ERROR here instead.
          (nix:posix-error errno)))))

;;;; Resolver Conditions

(define-constant +resolver-error-map+
  '((:eai-again      . resolver-again-error)
    (:eai-fail       . resolver-fail-error)
    (:eai-noname     . resolver-no-name-error)
    (:eai-nodata     . resolver-no-name-error)
    (:eai-addrfamily . resolver-no-name-error)
    (:eai-service    . resolver-no-service-error))
  :test 'equal)

(defun resolver-error-condition (id)
  (cdr (assoc id +resolver-error-map+)))

(defun resolver-error-code (id)
  (foreign-enum-value 'addrinfo-errors id))

(define-condition resolver-error (system-error)
  ((data :initarg :data :reader resolver-error-data))
  (:documentation
   "Signaled when an error occurs while trying to resolve an address."))

(defmacro define-resolver-error (name code identifier format-string
                                 &optional documentation)
  `(progn
     ;; (export ',name)
     (define-condition ,name (resolver-error)
       ((code :initform ,code)
        (identifier :initform ,identifier))
       (:report (lambda (condition stream)
                  (format stream ,format-string (resolver-error-data condition))
                  (print-message-if-not-null condition stream)))
       (:documentation ,documentation))))

(define-resolver-error resolver-again-error (resolver-error-code :eai-again)
  :resolver-again
  "Temporary failure occurred while resolving: ~S"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error (resolver-error-code :eai-fail)
  :resolver-fail
  "Non recoverable error occurred while resolving: ~S"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error (resolver-error-code :eai-noname)
  :resolver-no-name
  "Host or service not found: ~S"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-no-service-error
    (resolver-error-code :eai-service) :resolver-no-service
  "Service not found for specific socket type: ~S"
  "Condition signaled when a service was not found for the socket type
requested.")

(define-resolver-error resolver-unknown-error 0 :resolver-unknown
  "Unknown error while resolving: ~S"
  "Condition signaled when an unknown error is signaled while resolving
an address.")

(defun resolver-error (identifier &key data message)
  (let ((condition-class (resolver-error-condition identifier)))
    (if condition-class
        (error condition-class
               :code (resolver-error-code identifier)
               :identifier identifier
               :data data
               :message message)
        (error 'resolver-unknown-error
               :code (or (ignore-errors
                           (resolver-error-code identifier))
                         0)
               :identifier identifier
               :data data
               :message message))))

;;; For use with ERRNO-WRAPPER.
(defun signal-resolver-error (retval)
  (let ((identifier (foreign-enum-keyword 'addrinfo-errors retval)))
    (if (eq identifier :eai-system)
        (nix:posix-error)
        (resolver-error identifier))))
