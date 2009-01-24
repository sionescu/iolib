;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :io.multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :cffi)
  :pathname (merge-pathnames #p"io.multiplex/" *load-truename*)
  :components
  ((:file "pkgdcl")

   ;; Scheduler
   (:file "time" :depends-on ("pkgdcl"))
   (:file "utils" :depends-on ("pkgdcl" "time"))
   (:file "timers" :depends-on ("pkgdcl" "time"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))

   ;; Event loop
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "time" "timers" "queue"
                                    "scheduler" "fd-entry" "multiplexer"))

   ;; FD wait
   (:file "fd-wait" :depends-on ("pkgdcl" "utils"))

   ;; Event sources
   (:file "backend-select"
          :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+linux
   (:file "backend-epoll"
          :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+bsd
   (:file "backend-kqueue"
          :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   (:file "detect"
          :depends-on ("pkgdcl" "multiplexer" "backend-select"
                       #+linux "backend-epoll" #+bsd "backend-kqueue"))))
