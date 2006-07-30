;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(defpackage #:iolib-system
  (:use #:common-lisp #:asdf))

(in-package #:iolib-system)

(sb-alien:with-alien ((a (array (sb-alien:unsigned 8) 4)))
  (dotimes (i 4)
    (setf (sb-alien:deref a i) (1+ i)))
  (pushnew (case (sb-alien:deref (sb-alien:cast (sb-alien:addr a) (* sb-alien:unsigned)))
             (#x01020304 :big-endian)
             (#x04030201 :little-endian)
             (otherwise
              (error "Your machine seems to be neither little-endian nor big-endian. Please report this to the maintainer(s).")))
           *features*))

(defsystem iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:iolib-alien-ng
               #:net.sockets))
