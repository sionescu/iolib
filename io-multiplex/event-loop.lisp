;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; event-loop.lisp --- Main event loop.
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

(declaim (optimize (debug 3) (safety 3)))


;;;; EVENT-BASE

(defclass event-base ()
  ((mux :initarg :mux
        :reader mux-of)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds-of)
   (timers :initform (make-priority-queue :key #'%timer-expire-time)
           :reader timers-of)
   (fd-timers :initform (make-priority-queue :key #'%timer-expire-time)
              :reader fd-timers-of)
   (expired-events :initform nil
                   :accessor expired-events-of)
   (exit :initform nil
         :accessor exit-p)
   (exit-when-empty :initarg :exit-when-empty
                    :accessor exit-when-empty-p))
  (:default-initargs :mux (make-instance *default-multiplexer*)
                     :exit-when-empty nil)
  (:documentation "An event base ..."))

(defmacro with-event-base ((var &rest initargs) &body body)
  "Binds VAR to a new EVENT-BASE, instantiated with INITARGS,
within the extent of BODY.  Closes VAR."
  `(let ((,var (make-instance 'event-base ,@initargs)))
     (unwind-protect
          (locally ,@body)
       (when ,var (close ,var)))))

(defmethod print-object ((base event-base) stream)
  (print-unreadable-object (base stream :type nil :identity t)
    (if (fds-of base)
        (format stream "event base, ~A FDs monitored, using: ~A"
                (hash-table-count (fds-of base)) (mux-of base))
        (format stream "event base, closed"))))

(defmethod initialize-instance :after ((base event-base) &key)
  (with-slots (mux) base
    (when (symbolp mux)
      (setf mux (make-instance mux)))))

;;; KLUDGE: CLOSE is for streams. --luis
;;;
;;; Also, we might want to close FDs here.  Or have a version/argument
;;; that handles that.  Or... add finalizers to the fd streams.
(defmethod close ((event-base event-base) &key abort)
  (declare (ignore abort))
  (with-accessors ((mux mux-of)) event-base
    (close-multiplexer mux)
    (dolist (slot '(mux fds timers fd-timers expired-events))
      (setf (slot-value event-base slot) nil))
    (values event-base)))

(defgeneric add-fd (base fd event-type function &key timeout one-shot)
  (:documentation ""))

(defgeneric add-timer (event-base function timeout &key one-shot)
  (:documentation ""))

(defgeneric remove-event (event-base event)
  (:documentation ""))

(defgeneric remove-fd (event-base fd)
  (:documentation ""))

(defgeneric event-dispatch (event-base &key one-shot timeout &allow-other-keys)
  (:documentation ""))

(defgeneric exit-event-loop (event-base &key delay)
  (:documentation "")
  (:method ((event-base event-base) &key (delay 0))
    (add-timer event-base
               #'(lambda () (setf (exit-p event-base) t))
               delay :one-shot t)))

(defgeneric event-base-empty-p (event-base)
  (:documentation "Return T if no FD event or timeout is registered with EVENT-BASE.")
  (:method ((event-base event-base))
    (and (zerop (hash-table-count (fds-of event-base)))
         (priority-queue-empty-p (timers-of event-base)))))

;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

(defun fd-entry-of (event-base fd)
  "Return the FD-ENTRY of FD in EVENT-BASE."
  (gethash fd (fds-of event-base)))

(defun (setf fd-entry-of) (fd-entry event-base fd)
  "Return the FD-ENTRY of FD in EVENT-BASE."
  (setf (gethash fd (fds-of event-base)) fd-entry))

(defun remove-fd-entry (event-base fd)
  "Remove the FD-ENTRY of FD from EVENT-BASE."
  (remhash fd (fds-of event-base)))

;;;;;;;;;;;;;;;;;
;;; Internals ;;;
;;;;;;;;;;;;;;;;;

(defun expire-event (event-base event)
  (push event (expired-events-of event-base)))

(defun %add-fd-timer (event-base timer)
  (schedule-timer (fd-timers-of event-base) timer))

(defun %add-fd (event-base event fd-entry timeout)
  (with-accessors ((fd-timers fd-timers-of)) event-base
    (let ((fd (fd-event-fd event)))
      (when timeout
        (let ((timer (make-timer #'(lambda () (expire-event event-base event))
                                 timeout)))
          (setf (fd-event-timer event) timer)
          (%add-fd-timer event-base timer)))
      (setf (fd-entry-event fd-entry (fd-event-type event)) event)
      (setf (fd-entry-of event-base fd) fd-entry)
      (values event))))

(defmethod add-fd :before ((event-base event-base) fd event-type function
                           &key timeout one-shot)
  (declare (ignore timeout))
  (check-type fd unsigned-byte)
  (check-type event-type fd-event-type)
  (check-type function (or symbol function))
  (check-type one-shot boolean)
  (let ((fd-limit (fd-limit-of (mux-of event-base))))
    (when (and fd-limit (> fd fd-limit))
      (error "Cannot add such a large FD: ~A" fd))))

(defmethod add-fd ((event-base event-base) fd event-type function
                   &key timeout one-shot)
  (let ((current-entry (fd-entry-of event-base fd))
        (event (make-event fd event-type function one-shot)))
    (cond (current-entry
           (assert (null (fd-entry-event current-entry event-type))
                    ((fd-entry-event current-entry event-type))
                    "FD ~A is already monitored for event ~A" fd event-type)
           (%add-fd event-base event current-entry timeout)
           (update-fd (mux-of event-base) current-entry event-type :add))
          (t
           (let ((new-fd-entry (make-fd-entry fd)))
             (%add-fd event-base event new-fd-entry timeout)
             (monitor-fd (mux-of event-base) new-fd-entry))))
    (values event)))

(defun %add-timer (event-base timer)
  (schedule-timer (timers-of event-base) timer))

(defmethod add-timer ((event-base event-base) function
                      timeout &key one-shot)
  (check-type function (or symbol function))
  (check-type one-shot boolean)
  (%add-timer event-base (make-timer function timeout :one-shot one-shot)))

(defun %remove-fd-timer (event-base timer)
  (unschedule-timer (fd-timers-of event-base) timer))

(defun %remove-fd (event-base event)
  (with-accessors ((timers timers-of)) event-base
    (let* ((fd (fd-event-fd event))
           (fd-entry (fd-entry-of event-base fd)))
      (assert fd-entry (fd-entry) "FD ~A does not have an FD-ENTRY" fd)
      (setf (fd-entry-event fd-entry (fd-event-type event)) nil)
      (when-let ((timer (fd-event-timer event)))
        (%remove-fd-timer event-base timer))
      (when (fd-entry-empty-p fd-entry)
        (remove-fd-entry event-base fd))
      (values event))))

(defun %remove-fd-event (event-base event)
  (let* ((fd (fd-event-fd event))
         (current-entry (fd-entry-of event-base fd)))
    (cond (current-entry
           (%remove-fd event-base event)
           (if (fd-entry-empty-p current-entry)
               (unmonitor-fd (mux-of event-base) current-entry)
               (update-fd (mux-of event-base) current-entry
                          (fd-event-type event) :del)))
          (t 
           (%remove-fd event-base event)))))

(defun %remove-timer (event-base timer)
  (unschedule-timer (timers-of event-base) timer))

(defmethod remove-event ((event-base event-base) event)
  (etypecase event
    (timer    (%remove-timer event-base event))
    (fd-event (%remove-fd-event event-base event)))
  (values event-base))

(defun remove-events (event-base event-list)
  (dolist (ev event-list)
    (remove-event event-base ev)))

(defmethod remove-fd ((event-base event-base) fd)
  (let ((entry (fd-entry-of event-base fd)))
    (symbol-macrolet ((rev (fd-entry-read-event entry))
                      (wev (fd-entry-write-event entry))
                      (eev (fd-entry-error-event entry)))
      (labels ((maybe-remove-timer (event)
                 (when (and event (fd-event-timer event))
                   (%remove-fd-timer event-base (fd-event-timer event))))
               (maybe-remove-all-timers ()
                 (maybe-remove-timer rev)
                 (maybe-remove-timer wev)
                 (maybe-remove-timer eev)))
        (cond (entry
               (maybe-remove-all-timers)
               (unmonitor-fd (mux-of event-base) fd)
               (remove-fd-entry event-base fd))
              (t (warn "Trying to remove an unmonitored FD.")))))))

(defvar *maximum-event-loop-timeout* 1)

(defmethod event-dispatch :around ((event-base event-base)
                                   &key timeout one-shot)
  (declare (ignore one-shot))
  (setf (exit-p event-base) nil)
  (when timeout
    (exit-event-loop event-base :delay timeout))
  (call-next-method))

(defmethod event-dispatch ((event-base event-base) &key one-shot timeout
                           (max-timeout *maximum-event-loop-timeout*))
  (declare (ignore timeout))
  (with-accessors ((mux mux-of) (fds fds-of) (exit-p exit-p)
                   (exit-when-empty exit-when-empty-p)
                   (timers timers-of) (fd-timers fd-timers-of)
                   (expired-events expired-events-of))
      event-base
    (flet ((poll-timeout ()
             (min-timeout (time-to-next-timer timers)
                          (time-to-next-timer fd-timers)
                          max-timeout)))
      (do ((deletion-list () ())
           (got-fd-events-p nil nil)
           (got-fd-timeouts-p nil nil)
           (got-timers-p nil nil)
           (poll-timeout (poll-timeout) (poll-timeout))
           (now (osicat-sys:get-monotonic-time) (osicat-sys:get-monotonic-time)))
          ((or exit-p (and exit-when-empty (event-base-empty-p event-base))))
        (setf (expired-events-of event-base) nil)
        (setf (values got-fd-events-p deletion-list)
              (dispatch-fd-events-once event-base poll-timeout now))
        (remove-events event-base deletion-list)
        (setf got-fd-timeouts-p (expire-pending-timers fd-timers now))
        (dispatch-fd-timeouts expired-events)
        (setf got-timers-p (expire-pending-timers timers now))
        (when (and (or got-fd-events-p got-fd-timeouts-p got-timers-p)
                   one-shot)
          (setf exit-p t))))))

;;; Waits for events and dispatches them.  Returns T if some events
;;; have been received, NIL otherwise.
(defun dispatch-fd-events-once (event-base timeout now)
  (with-accessors ((mux mux-of) (fds fds-of) (fd-timers fd-timers-of))
      event-base
    (let ((deletion-list ())
          (fd-events (harvest-events mux timeout)))
      (dolist (ev fd-events)
        (destructuring-bind (fd ev-types) ev
          (let* ((fd-entry (fd-entry-of event-base fd))
                 (errorp (and fd-entry (member :error ev-types))))
            (labels ((append-events (events)
                       (nconcf deletion-list events))
                     (do-error ()
                       (%dispatch-event fd-entry :error now)
                       (append-events (fd-entry-all-events fd-entry)))
                     (do-read ()
                       (let ((events (%dispatch-event fd-entry :read now)))
                         (or errorp (append-events events))))
                     (do-write ()
                       (let ((events (%dispatch-event fd-entry :write now)))
                         (or errorp (append-events events)))))
              (cond (fd-entry
                     (when errorp (do-error))
                     (when (member :read ev-types) (do-read))
                     (when (member :write ev-types) (do-write)))
                    (t 
                     (warn "Got spurious event for non-monitored FD: ~A" fd)))))))
      (priority-queue-reorder fd-timers)
      (values (consp fd-events) deletion-list))))

(defun %dispatch-event (fd-entry event-type now)
  (let ((deletion-list ())
        (ev (fd-entry-event fd-entry event-type)))
    (funcall (fd-event-handler ev) (fd-entry-fd fd-entry) event-type)
    (when-let ((timer (fd-event-timer ev)))
      (reschedule-timer-relative-to-now timer now))
    (when (fd-event-one-shot-p ev) (push ev deletion-list))
    (values deletion-list)))

(defun dispatch-fd-timeouts (events)
  (dolist (ev events)
    (funcall (fd-event-handler ev)
             (fd-event-fd ev)
             :timeout)))
