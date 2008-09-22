;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- kequeue(2) multiplexer implementation.
;;;

(in-package :io.multiplex)

(defconstant +kqueue-priority+ 1)

(define-multiplexer kqueue-multiplexer +kqueue-priority+ (multiplexer)
  ())

(defmethod print-object ((mux kqueue-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "kqueue(2) multiplexer")))

(defvar *kqueue-max-events* 200)

(defmethod initialize-instance :after ((mux kqueue-multiplexer) &key)
  (setf (slot-value mux 'fd) (kqueue)))

(defun do-kqueue-event-request (kqueue-fd fd-entry filter request-type)
  (let ((fd (fd-entry-fd fd-entry)))
    (with-foreign-object (kev 'kevent)
      (bzero kev size-of-kevent)
      (ev-set kev fd filter request-type 0 0 (null-pointer))
      (kevent kqueue-fd
              kev 1
              (null-pointer) 0
              (null-pointer)))))

(defun calc-kqueue-monitor-filter (fd-entry)
  (if (null (fd-entry-read-handler fd-entry))
      evfilt-write
      evfilt-read))

(defmethod monitor-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-monitor-filter fd-entry)
                               ev-add)
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot monitor it." (fd-entry-fd fd-entry)))))

(defun calc-kqueue-update-filter-and-flags (event-type edge-change)
  (case event-type
    (:read
     (case edge-change
       (:add (values evfilt-read ev-add))
       (:del (values evfilt-read ev-delete))))
    (:write
     (case edge-change
       (:add (values evfilt-write ev-add))
       (:del (values evfilt-write ev-delete))))))

(defmethod update-fd ((mux kqueue-multiplexer) fd-entry event-type edge-change)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (handler-case
      (multiple-value-call #'do-kqueue-event-request (fd-of mux) fd-entry
                           (calc-kqueue-update-filter-and-flags event-type edge-change))
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot update its status."
            (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot update its status."
            (fd-entry-fd fd-entry)))))

(defun calc-kqueue-unmonitor-filter (fd-entry)
  (if (null (fd-entry-read-handler fd-entry))
      evfilt-read
      evfilt-write))

(defmethod unmonitor-fd ((mux kqueue-multiplexer) fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-unmonitor-filter fd-entry)
                               ev-delete)
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux kqueue-multiplexer) timeout)
  (with-foreign-objects ((events 'kevent *kqueue-max-events*)
                         (ts 'timespec))
    (bzero events (* *kqueue-max-events* size-of-kevent))
    (let (ready-fds)
      (nix:repeat-upon-condition-decreasing-timeout
          ((nix:eintr) tmp-timeout timeout)
        (when tmp-timeout
          (timeout->timespec tmp-timeout ts))
        (setf ready-fds
              (kevent (fd-of mux) (null-pointer) 0
                      events *kqueue-max-events*
                      (if tmp-timeout ts (null-pointer)))))
      (macrolet ((kevent-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'kevent i) 'kevent ',slot-name)))
        (loop for i below ready-fds
              for fd = (kevent-slot ident)
              for flags = (kevent-slot flags)
              for filter = (kevent-slot filter)
              for data = (kevent-slot data)
              for kqueue-event = (make-kqueue-event fd flags filter data)
              when kqueue-event collect kqueue-event)))))

;;; TODO: do something with DATA
(defun make-kqueue-event (fd flags filter data)
  (declare (ignore data))
  (let ((event ()))
    (switch (filter :test #'=)
      (evfilt-write (push :write event))
      (evfilt-read  (push :read event)))
    (flags-case flags
      ;; TODO: check what exactly EV_EOF means
      ;; (ev-eof   (pushnew :read event))
      (ev-error (push :error event)))
    (when event
      (list fd event))))
