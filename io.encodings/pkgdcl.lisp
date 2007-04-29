;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage :io.encodings
  (:nicknames #:ioenc)
  (:use #:common-lisp #:iolib-utils)
  (:export
   ;; External-format handling
   #:external-format #:make-external-format #:find-external-format
   #:ef-name #:ef-line-terminator #:ef-octet-size
   #:*external-format-list*
   #:*default-external-format* #:*default-line-terminator*
   #:octets-to-string #:string-to-octets
   ;; External format error conditions
   #:void-external-format
   #:octets-encoding-error
   #:octet-decoding-error #:end-of-input-in-character
   #:malformed-multibyte-sequence #:invalid-utf-8-starter-byte
   #:invalid-utf-8-continuation-byte #:overlong-utf-8-sequence
   #:illegal-code-point))
