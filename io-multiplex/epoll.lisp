;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :io.multiplex)

(defconstant +epoll-priority+ 1)


(define-multiplexer epoll-multiplexer +epoll-priority+ (multiplexer)
  ())


(defmethod print-object ((mux epoll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "epoll(4) multiplexer")))


(defconstant +epoll-default-size-hint+ 25)
(defvar *epoll-max-events* 200)


(defmethod initialize-instance :after ((mux epoll-multiplexer)
                                       &key (size +epoll-default-size-hint+))
  (setf (slot-value mux 'fd) (et:epoll-create size)))


(defun calc-epoll-flags (fd-entry)
  (logior (if (not (queue-empty-p (fd-entry-read-events fd-entry))) et:epollin 0)
          (if (not (queue-empty-p (fd-entry-write-events fd-entry))) et:epollout 0)
          et:epollpri))


(defmethod monitor-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry)
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'et:epoll-event)
      (et:memset ev 0 et:size-of-epoll-event)
      (setf (foreign-slot-value ev 'et:epoll-event 'et:events) flags)
      (setf (foreign-slot-value (foreign-slot-value ev 'et:epoll-event 'et:data)
                                'et:epoll-data 'et:fd)
            fd)
      (handler-case
          (et:epoll-ctl (fd-of mux) et:epoll-ctl-add fd ev)
        (et:ebadf (err) (declare (ignore err))
          (warn "FD ~A is invalid, cannot monitor it." fd))
        (et:eexist (err) (declare (ignore err))
          (warn "FD ~A is already monitored." fd))))))


(defmethod update-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry)
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'et:epoll-event)
      (et:memset ev 0 et:size-of-epoll-event)
      (setf (foreign-slot-value ev 'et:epoll-event 'et:events) flags)
      (setf (foreign-slot-value (foreign-slot-value ev 'et:epoll-event 'et:data)
                                'et:epoll-data 'et:fd)
            fd)
      (handler-case
          (et:epoll-ctl (fd-of mux) et:epoll-ctl-mod fd ev)
        (et:ebadf (err) (declare (ignore err))
          (warn "FD ~A is invalid, cannot update its status." fd))
        (et:enoent (err) (declare (ignore err))
          (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))


(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (handler-case
      (et:epoll-ctl (fd-of mux)
                    et:epoll-ctl-del
                    (fd-entry-fd fd-entry)
                    (null-pointer))
    (et:ebadf (err) (declare (ignore err))
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (et:enoent (err) (declare (ignore err))
      (warn "FD ~A was not monitored, cannot unmonitor it." (fd-entry-fd fd-entry)))))


(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  (with-foreign-object (events 'et:epoll-event *epoll-max-events*)
    (et:memset events 0 (* *epoll-max-events* et:size-of-epoll-event))
    (let (ready-fds)
      (et:repeat-upon-condition-decreasing-timeout ((et:eintr)
                                                    tmp-timeout timeout)
        (setf ready-fds
              (et:epoll-wait (fd-of mux) events
                             *epoll-max-events*
                             (timeout->milisec tmp-timeout))))
      (macrolet ((epoll-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'et:epoll-event i)
                                        'et:epoll-event ',slot-name)))
        (return-from harvest-events
          (loop
             :for i :below ready-fds
             :for fd := (foreign-slot-value (epoll-slot et:data) 'et:epoll-data 'et:fd)
             :for event-mask := (epoll-slot et:events)
             :for epoll-event := (make-epoll-event fd event-mask)
             :when epoll-event :collect epoll-event))))))


(defun make-epoll-event (fd mask)
  (let ((event ()))
    (flags-case mask
      ((et:epollout et:epollhup)
       (push :write event))
      ((et:epollin et:epollpri et:epollhup)
       (push :read event))
      (et:epollerr
       (push :error event)))
    (when event
      (list fd event))))
