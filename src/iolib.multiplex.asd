;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib.multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :cffi)
  :pathname #-asdf2 (merge-pathnames "multiplex/" *load-truename*)
            #+asdf2 "multiplex/"
  :default-component-class iolib.base:cl-source-file
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
