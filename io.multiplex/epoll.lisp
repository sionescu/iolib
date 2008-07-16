;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- epoll(2) multiplexer implementation.
;;;

(in-package :io.multiplex)

(defconstant +epoll-priority+ 1)

(define-multiplexer epoll-multiplexer +epoll-priority+ (multiplexer)
  ())

(defmethod print-object ((mux epoll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "epoll(4) multiplexer")))

(defconstant +epoll-default-size-hint+ 25)
(defconstant +epoll-max-events+ 1024)

(defmethod initialize-instance :after ((mux epoll-multiplexer)
                                       &key (size +epoll-default-size-hint+))
  (setf (slot-value mux 'fd) (epoll-create size)))

(defun calc-epoll-flags (fd-entry)
  (logior (if (fd-entry-read-event fd-entry)
              epollin 0)
          (if (fd-entry-write-event fd-entry)
              epollout 0)
          epollpri))

(defmethod monitor-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'epoll-event)
      (bzero ev size-of-epoll-event)
      (setf (foreign-slot-value ev 'epoll-event 'events) flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev 'epoll-event 'data) 'epoll-data 'fd)
            fd)
      (handler-case
          (epoll-ctl (fd-of mux) epoll-ctl-add fd ev)
        (nix:ebadf ()
          (warn "FD ~A is invalid, cannot monitor it." fd))
        (nix:eexist ()
          (warn "FD ~A is already monitored." fd))))))

(defmethod update-fd ((mux epoll-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'epoll-event)
      (bzero ev size-of-epoll-event)
      (setf (foreign-slot-value ev 'epoll-event 'events) flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev 'epoll-event 'data) 'epoll-data 'fd)
            fd)
      (handler-case
          (epoll-ctl (fd-of mux) epoll-ctl-mod fd ev)
        (nix:ebadf ()
          (warn "FD ~A is invalid, cannot update its status." fd))
        (nix:enoent ()
          (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))

(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (handler-case
      (epoll-ctl (fd-of mux)
                 epoll-ctl-del
                 (fd-entry-fd fd-entry)
                 (null-pointer))
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  (with-foreign-object (events 'epoll-event +epoll-max-events+)
    (bzero events (* *epoll-max-events* size-of-epoll-event))
    (let (ready-fds)
      (nix:repeat-upon-condition-decreasing-timeout
          ((nix:eintr) tmp-timeout timeout)
        (setf ready-fds (epoll-wait (fd-of mux) events *epoll-max-events*
                                    (timeout->milisec tmp-timeout))))
      (macrolet ((epoll-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'epoll-event i)
                                        'epoll-event ',slot-name)))
        (return-from harvest-events
          (loop :for i :below ready-fds
                :for fd := (foreign-slot-value (epoll-slot data) 'epoll-data 'fd)
                :for event-mask := (epoll-slot events)
                :for epoll-event := (make-epoll-event fd event-mask)
                :when epoll-event :collect epoll-event))))))

(defun make-epoll-event (fd mask)
  (let ((event ()))
    (flags-case mask
      ((epollout epollhup)
       (push :write event))
      ((epollin epollpri epollhup)
       (push :read event))
      (epollerr
       (push :error event)))
    (when event
      (list fd event))))
