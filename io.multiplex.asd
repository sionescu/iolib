;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

;;; Need trivial-features to correctly handle the reader conditionals
;;; in the system definition form.
(eval-when (:load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel)
  (asdf:oos 'asdf:load-op :trivial-features)
  #+cmu (require :gray-streams))

(asdf:defsystem :io.multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:osicat :alexandria :series)
  :pathname (merge-pathnames #p"io.multiplex/" *load-truename*)
  :components
  ((:file "pkgdcl")

   ;; Foreign definitions
   (cffi-grovel:grovel-file "grovel" :depends-on ("pkgdcl"))
   (:file "foreign-unix" :depends-on ("pkgdcl" "grovel"))
   #+linux (:file "foreign-linux" :depends-on ("pkgdcl" "grovel"))
   #+bsd (:file "foreign-bsd" :depends-on ("pkgdcl" "grovel"))

   ;; Scheduler
   (:file "time" :depends-on ("pkgdcl"))
   (:file "utils" :depends-on ("pkgdcl" "grovel" "time"))
   (:file "timers" :depends-on ("pkgdcl" "time"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))

   ;; Event loop
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "time" "timers" "queue"
                                    "scheduler" "fd-entry" "multiplexer"))

   ;; FD wait
   (:file "fd-wait" :depends-on ("pkgdcl" "grovel" "foreign-unix" "utils"))

   ;; Event sources
   (:file "select" :depends-on ("pkgdcl" "utils" "grovel" "foreign-unix"
                                "fd-entry" "multiplexer"))
   #+linux (:file "epoll" :depends-on ("pkgdcl" "grovel" "foreign-linux"
                                       "utils" "fd-entry" "multiplexer"))
   #+bsd (:file "kqueue" :depends-on ("pkgdcl" "grovel" "foreign-bsd"
                                      "utils" "fd-entry" "multiplexer"))
   (:file "detect" :depends-on ("pkgdcl" "multiplexer" "select"
                                #+linux "epoll" #+bsd "kqueue"))))
