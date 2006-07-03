;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage #:iolib-system
  (:use #:common-lisp #:asdf #:sb-grovel))

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
  :depends-on (#:sb-posix)
  :components
  ((:module :sockets
    :components
    ((:file "defpackage")
     (:file "common" :depends-on ("defpackage"))
     (:file "config" :depends-on ("defpackage"))
     (:file "conditions" :depends-on ("defpackage"))
     (:file "iface" :depends-on ("defpackage" "conditions" "common"))
     (:file "address" :depends-on ("defpackage" "conditions" "common"))
     (:file "resolv" :depends-on ("defpackage" "common" "config" "conditions" "address"))
     (:file "base-sockets"
            :depends-on ("defpackage" "config" "common" "address" "resolv"))
     (:file "socket-options"
            :depends-on ("defpackage" "common" "base-sockets"))
     (:file "stream-sockets"
            :depends-on ("defpackage" "common" "config" "base-sockets"))
     (:file "datagram-sockets"
            :depends-on ("defpackage" "common" "config" "base-sockets"))
     (:file "unix-sockets"
            :depends-on ("defpackage" "common" "config" "base-sockets"
                         "stream-sockets" "datagram-sockets"))))))

(defsystem iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:net.sockets))
