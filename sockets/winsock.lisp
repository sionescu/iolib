;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; winsock.lisp --- CFFI bindings specific to Winsock.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:bsd-sockets)

(defctype dword :unsigned-long)
(defctype word :unsigned-short)
(defctype socket :unsigned-int)
(defctype group :unsigned-int)

(defsyscall "get_osfhandle" :int
  (fd :int))

(defsyscall "open_osfhandle" :int
  (handle :inptr)
  (flags :int))

(defcstruct wsa-data
  (version word)
  (high-version word)
  (description :char :count 257)
  (system-status :char :count 129)
  (max-sockets :unsigned-short)
  (max-udp-dg :unsigned-short)
  (vendor-info :string))

(define-socket-call ("WSAStartup" %wsa-startup) :int
  (version-requested word)
  (data wsa-data))

(defun wsa-startup (version-requested)
  (with-foreign-object (data wsa-data)
    (%wsa-startup version-requested data)))

(define-socket-call ("WSASocketA" wsa-socket) socket
  (af :int)
  (type :int)
  (protocol :int)
  (protocol-info :pointer)
  (g group)
  (flags dword))
