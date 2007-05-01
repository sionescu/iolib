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

(defconstant +kqueue-priority+ 1)


(define-multiplexer kqueue-multiplexer +kqueue-priority+ (multiplexer)
  ())


(defmethod print-object ((mux kqueue-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "kqueue(2) multiplexer")))


(defvar *kqueue-max-events* 200)


(defmethod initialize-instance :after ((mux kqueue-multiplexer) &key)
  (setf (slot-value mux 'fd) (et:kqueue)))


(defun do-kqueue-event-request (kqueue-fd fd-entry filter request-type)
  (let ((fd (fd-entry-fd fd-entry)))
    (with-foreign-object (kev 'et:kevent)
      (et:memset kev 0 et:size-of-kevent)
      (et:ev-set kev fd filter request-type 0 0 (null-pointer))
      (et:kevent kqueue-fd
                 kev 1
                 (null-pointer) 0
                 (null-pointer)))))


(defun calc-kqueue-monitor-filter (fd-entry)
  (if (queue-empty-p (fd-entry-read-events fd-entry))
      et:evfilt-write
      et:evfilt-read))


(defmethod monitor-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-monitor-filter fd-entry)
                               et:ev-add)
    (et:ebadf ()
      (warn "FD ~A is invalid, cannot monitor it." (fd-entry-fd fd-entry)))))


(defun calc-kqueue-update-filter-and-flags (edge-change)
  (case edge-change
    (:read-add  (values et:evfilt-read et:ev-add))
    (:read-del  (values et:evfilt-read et:ev-delete))
    (:write-add (values et:evfilt-write et:ev-add))
    (:write-del (values et:evfilt-write et:ev-delete))))


(defmethod update-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (multiple-value-bind (filter change)
          (calc-kqueue-update-filter-and-flags (fd-entry-edge-change fd-entry))
        (do-kqueue-event-request (fd-of mux) fd-entry filter change))
    (et:ebadf ()
      (warn "FD ~A is invalid, cannot update its status." (fd-entry-fd fd-entry)))
    (et:enoent ()
      (warn "FD ~A was not monitored, cannot update its status." (fd-entry-fd fd-entry)))))


(defun calc-kqueue-unmonitor-filter (fd-entry)
  (if (queue-empty-p (fd-entry-read-events fd-entry))
      et:evfilt-read
      et:evfilt-write))


(defmethod unmonitor-fd ((mux kqueue-multiplexer) fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-unmonitor-filter fd-entry)
                               et:ev-delete)
    (et:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (et:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it." (fd-entry-fd fd-entry)))))


(defmethod harvest-events ((mux kqueue-multiplexer) timeout)
  (with-foreign-objects ((events 'et:kevent *kqueue-max-events*)
                         (ts 'et:timespec))
    (et:memset events 0 (* *kqueue-max-events* et:size-of-kevent))
    (let (ready-fds)
      (et:repeat-upon-condition-decreasing-timeout ((et:eintr)
                                                    tmp-timeout timeout)
        (when tmp-timeout
          (timeout->timespec tmp-timeout ts))
        (setf ready-fds
              (et:kevent (fd-of mux) (null-pointer) 0
                         events *kqueue-max-events*
                         (if tmp-timeout ts (null-pointer)))))
      (macrolet ((kevent-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'et:kevent i)
                                        'et:kevent ',slot-name)))
        (loop
           :for i :below ready-fds
           :for fd := (kevent-slot et:ident)
           :for flags := (kevent-slot et:flags)
           :for filter := (kevent-slot et:filter)
           :for data := (kevent-slot et:data)
           :for kqueue-event := (make-kqueue-event fd flags filter data)
           :when kqueue-event :collect kqueue-event)))))


;; TODO: do something with DATA
(defun make-kqueue-event (fd flags filter data)
  (declare (ignore data))
  (let ((event ()))
    (case filter
      (#.et:evfilt-write (push :write event))
      (#.et:evfilt-read  (push :read event)))
    (flags-case flags
;; TODO: check what exactly EV_EOF means
;;       (et:ev-eof   (pushnew :read event))
      (et:ev-error (push :error event)))
    (when event
      (list fd event))))
