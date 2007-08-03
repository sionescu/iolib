;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; wait.lisp --- WaitForMultipleObjects()-based multiplexer.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;; <http://msdn2.microsoft.com/en-us/library/ms687025.aspx> has some
;;; suggestions on how to work around this limitation.
(defconstant +maximum-wait-objects+ 64)

(defconstant +wait-priority+ 3)

(define-multiplexer wait-multiplexer +wait-priority+ (multiplexer)
  ((read-fds :initform nil :accessor mux-read-fds)
   (write-fds :initform nil :accessor mux-write-fds))
  (:default-initargs :fd-limit +maximum-wait-objects+))

(defmethod print-object ((mux wait-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "WaitForMultipleObjects() multiplexer")))

(defmethod close-multiplexer progn ((mux wait-multiplexer))
  (setf (mux-read-fds mux) nil
        (mux-write-fds mux) nil))

(defmethod monitor-fd ((mux wait-multiplexer) fd-entry)
  (unless (queue-empty-p (fd-entry-read-events fd-entry))
    (push (fd-entry-fd fd-entry) (mux-read-fds mux)))
  (unless (queue-empty-p (fd-entry-write-events fd-entry))
    (push (fd-entry-fd fd-entry) (mux-read-fds mux)))
  t)

(defmethod update-fd ((mux wait-multiplexer) fd-entry)
  (if (queue-empty-p (fd-entry-read-events fd-entry))
      (alexandria:deletef (mux-read-fds mux) (fd-entry-fd fd-entry))
      (push (fd-entry-fd fd-entry) (mux-read-fds mux)))
  (if (queue-empty-p (fd-entry-write-events fd-entry))
      (alexandria:deletef (mux-write-fds mux) (fd-entry-fd fd-entry))
      (push (fd-entry-fd fd-entry) (mux-write-fds mux)))
  t)

(defmethod unmonitor-fd ((mux wait-multiplexer) fd-entry)
  (alexandria:deletef (mux-read-fds mux) (fd-entry-fd fd-entry))
  (alexandria:deletef (mux-write-fds mux) (fd-entry-fd fd-entry))
  t)

;;; FIXME: can we get WAIT_ABANDONED+X?  What to do in that case?
(defun wait-for-multiple-objects (fds timeout)
  ;; with a constant size we can do stack allocation.
  (with-foreign-object (handles :intptr +maximum-wait-objects+)
    (loop for i from 0 and fd in fds
          do (setf (mem-aref handles :intptr i)
                   (get-osfhandle fd)))
    (let ((ret (%wait (length fds) handles nil (timeout->milisec timeout))))
      (assert (not (eql ret +wait-failed+)))
      (if (< ret +wait-abandoned+)
          ret
          nil))))

(defmethod harvest-events ((mux wait-multiplexer) timeout)
  (let ((ret (wait-for-multiple-objects (mux-read-fds mux) timeout))
        (found nil))
    (nconc (loop for i from 0 and fd in (mux-write-fds mux) collect
                 (list fd (if (eql ret i)
                              (progn (setq found t)
                                     (list :write :read))
                              (list :write))))
           (unless found
             (list ret (list :read))))))
