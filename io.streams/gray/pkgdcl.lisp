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

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc.fasl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gray-stream-symbols*
    '(#:fundamental-stream #:fundamental-input-stream
      #:fundamental-output-stream #:fundamental-character-stream
      #:fundamental-binary-stream #:fundamental-character-input-stream
      #:fundamental-character-output-stream
      #:fundamental-binary-input-stream
      #:fundamental-binary-output-stream #:stream-read-char
      #:stream-unread-char #:stream-read-char-no-hang
      #:stream-peek-char #:stream-listen #:stream-read-line
      #:stream-clear-input #:stream-write-char #:stream-line-column
      #:stream-start-line-p #:stream-write-string #:stream-terpri
      #:stream-fresh-line #:stream-finish-output #:stream-force-output
      #:stream-clear-output #:stream-advance-to-column
      #:stream-read-byte #:stream-write-byte))

  (defparameter *gray-stream-package*
    #+allegro :excl
    #+cmu :ext
    #+clisp :gray
    #+ecl :gray
    #+(or ccl openmcl) :ccl
    #+lispworks :stream
    #+sbcl :sb-gray
    #-(or allegro cmu clisp ecl ccl openmcl lispworks sbcl)
    (error "Your CL implementation isn't supported.")))

(import (mapcar #'(lambda (s) (find-symbol (string s) *gray-stream-package*))
                *gray-stream-symbols*)
        :io.streams)
