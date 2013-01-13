;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.syscalls :cffi)
  :default-component-class :iolib-source-file
  :pathname "multiplex/"
  :components
  ((:file "pkgdcl")

   ;; Scheduler
   (:file "utils" :depends-on ("pkgdcl"))
   (:file "timers" :depends-on ("pkgdcl"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))

   ;; Event loop
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "timers" "queue" "scheduler"
                                    "fd-entry" "multiplexer"))

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
