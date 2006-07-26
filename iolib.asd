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

(defsystem net.sockets
  :description "Socket library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:iolib-alien-ng)
  :components
  ((:module :sockets
    :components
    ((:file "defpackage")
     (:file "common" :depends-on ("defpackage"))
     (:file "conditions" :depends-on ("defpackage" "common"))
     (:file "config" :depends-on ("defpackage" "common"))
     (:file "iface" :depends-on ("defpackage" "common" "conditions"))
     (:file "address" :depends-on ("defpackage" "common" "conditions"))
     (:file "resolv" :depends-on ("defpackage" "common"
                                  "config" "conditions" "address"))
     (:file "base-sockets" :depends-on ("defpackage" "config" "resolv"))
     (:file "socket-options" :depends-on ("defpackage" "common" "base-sockets"))
     (:file "socket-methods"
            :depends-on ("defpackage" "config" "common" "address"
                         "base-sockets" "socket-options"))
     (:file "make-socket"
            :depends-on ("defpackage" "config" "common" "address"
                         "base-sockets" "socket-options" "socket-methods"))))))

(defsystem iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:net.sockets))
