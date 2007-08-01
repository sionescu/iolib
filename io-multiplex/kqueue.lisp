;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; kqueue.lisp --- kequeue multiplexer implementation.
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

(defconstant +kqueue-priority+ 1)

(define-multiplexer kqueue-multiplexer +kqueue-priority+ (multiplexer)
  ())

(defmethod print-object ((mux kqueue-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "kqueue(2) multiplexer")))

(defvar *kqueue-max-events* 200)

(defmethod initialize-instance :after ((mux kqueue-multiplexer) &key)
  (setf (slot-value mux 'fd) (nix:kqueue)))

(defun do-kqueue-event-request (kqueue-fd fd-entry filter request-type)
  (let ((fd (fd-entry-fd fd-entry)))
    (with-foreign-object (kev 'nix::kevent)
      (cl-posix-ffi:memset kev 0 nix::size-of-kevent)
      (nix:ev-set kev fd filter request-type 0 0 (null-pointer))
      (nix:kevent kqueue-fd
                  kev 1
                  (null-pointer) 0
                  (null-pointer)))))

(defun calc-kqueue-monitor-filter (fd-entry)
  (if (queue-empty-p (fd-entry-read-events fd-entry))
      nix::evfilt-write
      nix::evfilt-read))

(defmethod monitor-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-monitor-filter fd-entry)
                               nix::ev-add)
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot monitor it." (fd-entry-fd fd-entry)))))

(defun calc-kqueue-update-filter-and-flags (edge-change)
  (case edge-change
    (:read-add  (values nix::evfilt-read nix::ev-add))
    (:read-del  (values nix::evfilt-read nix::ev-delete))
    (:write-add (values nix::evfilt-write nix::ev-add))
    (:write-del (values nix::evfilt-write nix::ev-delete))))

(defmethod update-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (multiple-value-bind (filter change)
          (calc-kqueue-update-filter-and-flags (fd-entry-edge-change fd-entry))
        (do-kqueue-event-request (fd-of mux) fd-entry filter change))
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot update its status."
            (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot update its status."
            (fd-entry-fd fd-entry)))))

(defun calc-kqueue-unmonitor-filter (fd-entry)
  (if (queue-empty-p (fd-entry-read-events fd-entry))
      nix::evfilt-read
      nix::evfilt-write))

(defmethod unmonitor-fd ((mux kqueue-multiplexer) fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-unmonitor-filter fd-entry)
                               nix::ev-delete)
    (nix:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (nix:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux kqueue-multiplexer) timeout)
  (with-foreign-objects ((events 'nix::kevent *kqueue-max-events*)
                         (ts 'nix::timespec))
    (cl-posix-ffi:memset events 0 (* *kqueue-max-events* nix::size-of-kevent))
    (let (ready-fds)
      (repeat-upon-condition-decreasing-timeout
          ((nix:eintr) tmp-timeout timeout)
        (when tmp-timeout
          (timeout->timespec tmp-timeout ts))
        (setf ready-fds
              (nix:kevent (fd-of mux) (null-pointer) 0
                          events *kqueue-max-events*
                          (if tmp-timeout ts (null-pointer)))))
      (macrolet ((kevent-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'nix::kevent i)
                                        'nix::kevent ',slot-name)))
        (loop for i below ready-fds
              for fd = (kevent-slot nix::ident)
              for flags = (kevent-slot nix::flags)
              for filter = (kevent-slot nix::filter)
              for data = (kevent-slot nix::data)
              for kqueue-event = (make-kqueue-event fd flags filter data)
              when kqueue-event collect kqueue-event)))))

;;; TODO: do something with DATA
(defun make-kqueue-event (fd flags filter data)
  (declare (ignore data))
  (let ((event ()))
    (case filter
      (#.nix::evfilt-write (push :write event))
      (#.nix::evfilt-read  (push :read event)))
    (flags-case flags
      ;; TODO: check what exactly EV_EOF means
      ;; (nix::ev-eof   (pushnew :read event))
      (nix::ev-error (push :error event)))
    (when event
      (list fd event))))
