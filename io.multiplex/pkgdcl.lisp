;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :io.multiplex
  (:nicknames #:iomux)
  (:use #:common-lisp :cffi :alexandria :series)
  (:import-from #:osicat-posix #:defsyscall #:bzero
                #:file-descriptor-designator
                #:timeval #:sec #:usec)
  (:shadowing-import-from :series #:let #:let* #:multiple-value-bind
                          #:funcall #:defun #:until #:collect)
  (:export
   ;; Classes
   #:event
   #:event-base
   #:multiplexer
   #:select-multiplexer
   #:poll-multiplexer
   #+bsd #:kqueue-multiplexer
   #+linux #:epoll-multiplexer
   #:fd-event
   #:priority-queue

   ;; Event-base Operations
   #:*available-multiplexers*
   #:*default-multiplexer*
   #:*default-event-loop-timeout*
   #:add-fd
   #:add-timer
   #:event-base-empty-p
   #:event-dispatch
   #:exit-event-loop
   #:remove-event
   #:remove-fd
   #:with-event-base

   ;; Operations on FDs
   #:fd-readablep
   #:fd-ready-p
   #:fd-writablep
   #:poll-error
   #:poll-error-fd
   #:poll-error-identifier
   #:wait-until-fd-ready
   #:poll-timeout
   #:poll-timeout-fd
   #:poll-timeout-event-type
   ))
