;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :io.streams
  (:use :iolib.base :cffi)
  (:export
   ;; Classes
   #:dual-channel-fd-mixin
   #:dual-channel-gray-stream
   #:dual-channel-single-fd-mixin
   #:dual-channel-single-fd-gray-stream

   ;; Types
   #:sb16
   #:sb32
   #:sb8
   #:ub16
   #:ub16-sarray
   #:ub32
   #:ub8
   #:ub8-sarray
   #:ub8-vector

   ;; Accessors
   #:external-format-of
   #:fd-non-blocking
   #:fd-of
   #:input-fd-non-blocking
   #:input-fd-of
   #:output-fd-non-blocking
   #:output-fd-of
   #:read-buffer-size
   #:read-buffer-empty-p
   #:write-buffer-size
   #:write-buffer-empty-p

   #:read-sequence*
   #:write-sequence*
   #:drain-input-buffer
   ))
