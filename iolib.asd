;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage #:iolib-system
  (:use #:common-lisp #:asdf #:sb-grovel))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:iolib-alien
    (:nicknames #:et)
    (:use #:common-lisp #:sb-alien)
    (:shadow #:listen)))
(in-package #:iolib-system)

(defsystem iolib-alien
  :description "Alien definitions for IOLib."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "public domain"
  :depends-on (#:sb-grovel
               #:sb-posix)
  :components
  ((sb-grovel:grovel-constants-file
       "alien-constants"
       :package #:iolib-alien)
   (:file "alien-functions" :depends-on ("alien-constants"))))

(defsystem iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (#:iolib-alien))
