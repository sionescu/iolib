;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; wrappers.lisp --- Wrappers for initialization of GNUTLS and GCRYPT.
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

(include "gcrypt.h" "errno.h" "pthread.h")

(c "GCRY_THREAD_OPTION_PTHREAD_IMPL;")

(defwrapper* "gcrypt_set_thread_cbs"
    ("gcry_error_t" (errno-wrapper :unsigned-int
                                   :error-predicate plusp
                                   :error-generator signal-gpg-error))
  ()
  "return gcry_control(GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);")
