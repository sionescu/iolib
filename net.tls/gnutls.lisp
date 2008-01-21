;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; gnutls.lisp --- Bindings to GNUTLS 2.2.0(and compatible).
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :net.tls)

(defmacro defcfun* (name return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name)
    `(defcfun (,c-name ,lisp-name ,@options) ,return-type
       ,@args)))

(defmacro define-gnutls-function (name return-type &body args)
  `(defcfun* ,name
       (errno-wrapper ,return-type :error-generator signal-gnutls-error)
     ,@args))

(defcfun* (%gnutls-check-version "gnutls_check_version")
    (errno-wrapper :string
                   :error-predicate (lambda (r) (not (stringp r)))
                   :error-generator (lambda (r)
                                      (declare (ignore r))
                                      (error "Need GNUTLS version >= ~A"
                                             required-version)))
  (required-version :string))

(defcfun* (%gnutls_strerror "gnutls_strerror") :string
  (error :int))

;;; Global state functions

(define-gnutls-function (%gnutls-global-init "gnutls_global_init") :int)

(defcfun* (%gnutls-global-deinit "gnutls_global_deinit") :void)

;;; Session functions

(defctype gnutls-session :pointer)

(define-gnutls-function (%gnutls-init "gnutls_init") :int
  (session :pointer) ; gnutls-session *
  (con-end gnutls-connection-end))

(defcfun* (%gnutls-deinit "gnutls_deinit") :void
  (session gnutls-session))

(define-gnutls-function (%gnutls-bye "gnutls_bye") :int
  (session gnutls-session)
  (how     gnutls-close-request))

(define-gnutls-function (%gnutls-handshake "gnutls_handshake") :int
  (session gnutls-session))

(define-gnutls-function (%gnutls-rehandshake "gnutls_rehandshake") :int
  (session gnutls-session))

;;; Transport functions

(defctype gnutls-transport-ptr :pointer) ; void *
(defctype gnutls-pull-func :pointer) ; ssize_t (gnutls_transport_ptr_t, void *, size_t)
(defctype gnutls-push-func :pointer) ; ssize_t (gnutls_transport_ptr_t, void *, size_t)

(defcfun* (%gnutls-transport-set-ptr "gnutls_transport_set_ptr") :void
  (session gnutls-session)
  (ptr     gnutls-transport-ptr))

(defcfun* (%gnutls-transport-set-ptr2 "gnutls_transport_set_ptr2") :void
  (session  gnutls-session)
  (recv-ptr gnutls-transport-ptr)
  (send-ptr gnutls-transport-ptr))

(defcfun* (%gnutls-transport-get-ptr "gnutls_transport_get_ptr") gnutls-transport-ptr
  (session gnutls-session))

(defcfun* (%gnutls-transport-get-ptr2 "gnutls_transport_get_ptr2") :void
  (session  gnutls-session)
  (recv-ptr :pointer)  ; gnutls-transport-ptr *
  (send-ptr :pointer)) ; gnutls-transport-ptr *

(defcfun* (%gnutls-transport-set-lowat "gnutls_transport_set_lowat") :void
  (session gnutls-session)
  (num     :int))

(defcfun* (%gnutls-transport-set-push-function "gnutls_transport_set_push_function") :void
  (session   gnutls-session)
  (push-func gnutls-push-func))

(defcfun* (%gnutls-transport-set-pull-function "gnutls_transport_set_pull_function") :void
  (session   gnutls-session)
  (pull-func gnutls-pull-func))

(defcfun* (%gnutls-transport-set-errno "gnutls_transport_set_errno") :void
  (session gnutls-session)
  (err     :int))

(defcfun* (%gnutls-transport-set-global-errno "gnutls_transport_set_global_errno") :void
  (err :int))

;;; Record-level functions

(define-gnutls-function (%gnutls_record_send "gnutls_record_send") ssize
  (session gnutls-session)
  (data    :pointer) ; void *
  (nbytes  size))

(define-gnutls-function (%gnutls_record_recv "gnutls_record_recv") ssize
  (session gnutls-session)
  (data    :pointer) ; void *
  (nbytes  size))

(defcfun* (%gnutls_record_get_direction "gnutls_record_get_direction") :int
  (session gnutls-session))

(defcfun* (%gnutls_record_get_max_size "gnutls_record_get_max_size") size
  (session gnutls-session))

(defcfun* (%gnutls_record_set_max_size "gnutls_record_set_max_size") ssize
  (session gnutls-session)
  (size    size))

;;; Priority functions

(defctype gnutls-priority :pointer)

(define-gnutls-function (%gnutls-priority-init "gnutls_priority_init") :int
  (prio     :pointer)  ; gnutls-priority *
  (priority :string)
  (err-pos  :pointer)) ; const char **

(defcfun* (%gnutls-priority-deinit "gnutls_priority_deinit") :void
  (prio :pointer)) ; gnutls-priority *

(define-gnutls-function (%gnutls-priority-set "gnutls_priority_set") :int
  (session gnutls-session)
  (prio    gnutls-priority))

(define-gnutls-function (%gnutls-priority-set-direct "gnutls_priority_set_direct") :int
  (session  gnutls-session)
  (priority :string)
  (err-pos  :pointer)) ; const char **

;;; Credentials functions

(defctype gnutls-anon-client-credentials :pointer)
(defctype gnutls-anon-server-credentials :pointer)
(defctype gnutls-certificate-client-credentials :pointer)
(defctype gnutls-certificate-server-credentials :pointer)

(define-gnutls-function (%gnutls-credentials-set "gnutls_credentials_set") :int
  (session gnutls-session)
  (type    gnutls-credentials-type)
  (cred    :pointer)) ; gnutls-*-*-credentials *

(defcfun* (%gnutls-credentials-clear "gnutls_credentials_clear") :void
  (session gnutls-session))

(defcfun* (%gnutls_anon_free_client_credentials "gnutls_anon_free_client_credentials") :void
  (cred gnutls-anon-client-credentials))

(defcfun* (%gnutls_anon_free_server_credentials "gnutls_anon_free_server_credentials") :void
  (cred gnutls-anon-server-credentials))
