;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; epoll.lisp --- epoll()-based multiplexer implementation.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

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
  (setf (slot-value mux 'fd) (nix:epoll-create size)))

(defun calc-epoll-flags (fd-entry)
  (logior (if (not (queue-empty-p (fd-entry-read-events fd-entry)))
              nix::epollin 0)
          (if (not (queue-empty-p (fd-entry-write-events fd-entry)))
              nix::epollout 0)
          nix::epollpri))

(defmethod monitor-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry)
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'nix::epoll-event)
      (nix:bzero ev nix::size-of-epoll-event)
      (setf (foreign-slot-value ev 'nix::epoll-event 'nix::events)
            flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev 'nix::epoll-event 'nix::data)
             'nix::epoll-data 'nix::fd)
            fd)
      (handler-case
          (nix:epoll-ctl (fd-of mux) nix:epoll-ctl-add fd ev)
        (nix:ebadf ()
          (warn "FD ~A is invalid, cannot monitor it." fd))
        (nix:eexist ()
          (warn "FD ~A is already monitored." fd))))))

(defmethod update-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry)
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'nix::epoll-event)
      (nix:bzero ev nix::size-of-epoll-event)
      (setf (foreign-slot-value ev 'nix::epoll-event 'nix::events)
            flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev 'nix::epoll-event 'nix::data)
             'nix::epoll-data 'nix::fd)
            fd)
      (handler-case
          (nix:epoll-ctl (fd-of mux) nix::epoll-ctl-mod fd ev)
        (nix:ebadf ()
          (warn "FD ~A is invalid, cannot update its status." fd))
        (nix:enoent ()
          (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))

(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (handler-case
      (nix:epoll-ctl (fd-of mux)
                     nix::epoll-ctl-del
                     (fd-entry-fd fd-entry)
                     (null-pointer))
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  (with-foreign-object (events 'nix::epoll-event *epoll-max-events*)
    (nix:bzero events (* *epoll-max-events* nix::size-of-epoll-event))
    (let (ready-fds)
      (nix:repeat-upon-condition-decreasing-timeout
          ((nix:eintr) tmp-timeout timeout)
        (setf ready-fds (nix:epoll-wait (fd-of mux) events *epoll-max-events*
                                        (timeout->milisec tmp-timeout))))
      (macrolet ((epoll-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'nix::epoll-event i)
                                        'nix::epoll-event ',slot-name)))
        (return-from harvest-events
          (loop for i below ready-fds
                for fd = (foreign-slot-value (epoll-slot nix::data)
                                             'nix::epoll-data 'nix::fd)
                for event-mask = (epoll-slot nix::events)
                for epoll-event = (make-epoll-event fd event-mask)
                when epoll-event collect epoll-event))))))

(defun make-epoll-event (fd mask)
  (let ((event ()))
    (flags-case mask
      ((nix::epollout nix::epollhup)
       (push :write event))
      ((nix::epollin nix::epollpri nix::epollhup)
       (push :read event))
      (nix::epollerr
       (push :error event)))
    (when event
      (list fd event))))
