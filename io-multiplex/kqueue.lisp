;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006,2007 by Stelian Ionescu                            ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :io.multiplex)

(defconstant +kqueue-priority+ 1)

(define-multiplexer kqueue-multiplexer +kqueue-priority+
  (multiplexer)
  ((kqueue-fd :reader kqueue-fd)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *kqueue-max-events* 200))

(defmethod initialize-instance :after ((mux kqueue-multiplexer) &key)
  (let ((kqueue-fd (et:kqueue)))
    (setf (slot-value mux 'kqueue-fd) kqueue-fd)
    (finalize-object-closing-fd mux kqueue-fd)))

(defun calc-kqueue-filter (fd-entry)
  (logior (if (fd-entry-read-handlers fd-entry) et:evfilt-read 0)
          (if (fd-entry-write-handlers fd-entry) et:evfilt-write 0)))

(defun do-kqueue-event-request (kqueue-fd fd-entry request-type)
  (let ((filter (calc-kqueue-filter fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (kev 'et:kevent)
      (et:memset kev 0 #.(foreign-type-size 'et:kevent))
      (et:ev-set kev fd filter request-type 0 0 (null-pointer))
      (et:kevent kqueue-fd
                 kev 1
                 (null-pointer) 0
                 (null-pointer)))))

(defmethod monitor-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (do-kqueue-event-request (kqueue-fd mux) fd-entry et:ev-add)
    (et:unix-error-badf (err)
      (declare (ignore err))
      (warn "FD ~A is invalid, cannot monitor it." (fd-entry-fd fd-entry))
      (return-from monitor-fd nil))))

(defmethod update-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry)
  (handler-case
      (do-kqueue-event-request (kqueue-fd mux) fd-entry et:ev-add)
    (et:unix-error-badf (err)
      (declare (ignore err))
      (warn "FD ~A is invalid, cannot update its status." (fd-entry-fd fd-entry))
      (return-from update-fd nil))
    (et:unix-error-noent (err)
      (declare (ignore err))
      (warn "FD ~A was not monitored, cannot update its status." (fd-entry-fd fd-entry))
      (return-from update-fd nil)))
  t)

(defmethod unmonitor-fd ((mux kqueue-multiplexer) fd &key)
  (handler-case
      (do-kqueue-event-request (kqueue-fd mux) (fd-entry mux fd) et:ev-delete)
    (et:unix-error-badf (err)
      (declare (ignore err))
      (warn "FD ~A is invalid, cannot unmonitor it." fd))
    (et:unix-error-noent (err)
      (declare (ignore err))
      (warn "FD ~A was not monitored, cannot unmonitor it." fd)))
  t)

(defun kqueue-serve-single-fd (fd-entry flags events data)
  (assert fd-entry)
  (let ((error-handlers (fd-entry-error-handlers fd-entry))
        (read-handlers (fd-entry-read-handlers fd-entry))
        (write-handlers (fd-entry-write-handlers fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (when (and error-handlers (logtest et:ev-error flags))
      (dolist (error-handler error-handlers)
        (funcall (handler-function error-handler) fd :error)))
    (when (and read-handlers (logtest et:evfilt-read events))
      (dolist (read-handler (fd-entry-read-handlers fd-entry))
        (funcall (handler-function read-handler) fd :read)))
    (when (and write-handlers (logtest et:evfilt-write events))
      (dolist (write-handler (fd-entry-write-handlers fd-entry))
        (funcall (handler-function write-handler) fd :write)))))

;; (defmethod serve-fd-events ((mux kqueue-multiplexer)
;;                             &key timeout)
;;   (with-foreign-objects ((events 'et:kevent #.*kqueue-max-events*)
;;                          (ts 'et:timespec))
;;     (et:memset events 0 #.(* *kqueue-max-events* (foreign-type-size 'et:kevent)))
;;     (when timeout
;;       (with-foreign-slots ((et:tv-sec et:tv-nsec) ts et:timespec)
;;         (setf et:tv-sec (timeout-sec timeout)
;;               et:tv-nsec (* 1000 (timeout-usec timeout)))))
;;     (let ((ready-fds
;;            (et:kevent (kqueue-fd mux) (null-pointer) 0
;;                       events #.*kqueue-max-events* (if timeout ts (null-pointer)))))
;;       (loop
;;          :for i :below ready-fds
;;          :for fd := (foreign-slot-value (foreign-slot-value (mem-aref events 'et:kevent i)
;;                                                             'et:epoll-event 'et:data)
;;                                         'et:epoll-data 'et:fd)
;;          :for event-mask := (foreign-slot-value (mem-aref events 'et:epoll-event i)
;;                                                 'et:epoll-event 'et:events)
;;          :do (epoll-serve-single-fd (fd-entry mux fd)
;;                                     event-mask))
;;       (return-from serve-fd-events ready-fds))))

(defmethod close-multiplexer ((mux kqueue-multiplexer))
  (cancel-finalization mux)
  (et:close (kqueue-fd mux)))
