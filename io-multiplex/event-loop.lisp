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


;;;; EVENT-BASE

(defclass event-base ()
  ((mux :initform (make-instance *default-multiplexer*)
        :initarg :mux :reader mux-of)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds-of)
   (timeouts :initform (make-queue)
             :reader timeouts-of)
   (exit :initform nil
         :accessor exit-p)
   (exit-when-empty :initarg :exit-when-empty
                    :accessor exit-when-empty-p))
  (:default-initargs :exit-when-empty nil)
  (:documentation "An event base ..."))

(defmacro with-event-base ((var &rest initargs) &body body)
  "Binds VAR to a new EVENT-BASE, instantiated with INITARGS,
within the extent of BODY.  Closes VAR."
  `(let ((,var (make-instance 'event-base ,@initargs)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

(defmethod print-object ((base event-base) stream)
  (print-unreadable-object (base stream :type nil :identity t)
    (format stream "event base, ~A FDs monitored, using: ~A"
            ;; kludge: quick fix for printing closed event bases
            (when (fds-of base) (hash-table-count (fds-of base)))
            (mux-of base))))

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
    (dolist (slot '(fds timeouts exit))
      (setf (slot-value event-base slot) nil))
    (values event-base)))

(defgeneric add-fd (base fd event-type function &key timeout one-shot)
  (:documentation ""))

(defgeneric add-timeout (event-base function timeout &key persistent)
  (:documentation ""))

(defgeneric remove-event (event-base event)
  (:documentation ""))

(defgeneric remove-events (event-base event-list)
  (:documentation ""))

(defgeneric event-dispatch (event-base &key timeout one-shot &allow-other-keys)
  (:documentation ""))

(defgeneric exit-event-loop (event-base &key delay)
  (:documentation "")
  (:method ((event-base event-base) &key (delay 0))
    (add-timeout event-base
                 #'(lambda (fd event-type)
                     (declare (ignore fd event-type))
                     (setf (exit-p event-base) t))
                 delay :persistent nil)))

(defgeneric event-base-empty-p (event-base)
  (:documentation "Return T if no FD event or timeout is registered with EVENT-BASE.")
  (:method ((event-base event-base))
    (and (zerop (hash-table-count (fds-of event-base)))
         (queue-empty-p (timeouts-of event-base)))))

(defgeneric fd-entry-of (event-base fd)
  (:documentation "Return the FD-ENTRY of FD in EVENT-BASE.")
  (:method ((event-base event-base) fd)
    (gethash fd (fds-of event-base))))

(defun %add-event (event-base event &optional fd-entry)
  (with-accessors ((fds fds-of) (timeouts timeouts-of)) event-base
    (when (event-timeout event)
      ;; add the event to the timeout queue
      (queue-sorted-insert timeouts event #'< #'event-abs-timeout))
    (let ((fd (event-fd event)))
      ;; if it's an FD event add it to its fd-entry int the FDs hash-table
      ;; if there's no such fd-entry, create it
      (when fd
        (fd-entry-add-event fd-entry event)
        (setf (gethash fd fds) fd-entry))
      (values event))))

(defun %remove-event (event-base event)
  (with-accessors ((fds fds-of) (timeouts timeouts-of)) event-base
    (when (event-timeout event)
      ;; remove the event from the timeout queue
      (queue-delete timeouts event))
    (let ((fd (event-fd event)))
      ;; if it's an FD event remove it from its fd-entry
      ;; if the fd-emtry is then empty, remove it
      (when fd
        (let ((fd-entry (gethash fd fds)))
          (assert fd-entry)
          (fd-entry-del-event fd-entry event)
          (when (fd-entry-empty-p fd-entry)
            (remhash fd fds))))
      (values event))))

(defun calc-possible-edge-change-when-adding (fd-entry event-type)
  (cond ((and (eql event-type :read)
              (queue-empty-p (fd-entry-read-events fd-entry)))
         :read-add)
        ((and (eql event-type :write)
              (queue-empty-p (fd-entry-write-events fd-entry)))
         :write-add)))

(defmethod add-fd ((event-base event-base) fd event-type function
                   &key timeout one-shot)
  (check-type fd unsigned-byte)
  (check-type event-type fd-event)
  (let ((fd-limit (fd-limit-of (mux-of event-base))))
    (when (and fd-limit (> fd fd-limit))
      (error "Cannot add such a large FD: ~A" fd)))
  (let ((current-entry (fd-entry-of event-base fd))
        (event (make-event fd event-type function (not one-shot)
                           (abs-timeout timeout)
                           (normalize-timeout timeout)))
        (edge-change nil))
    (if current-entry
        (progn
          (setf edge-change (calc-possible-edge-change-when-adding
                             current-entry event-type))
          (%add-event event-base event current-entry)
          (when edge-change
            (setf (fd-entry-edge-change current-entry) edge-change)
            (update-fd (mux-of event-base) current-entry)
            (setf (fd-entry-edge-change current-entry) nil)))
        (progn
          (setf current-entry (make-fd-entry fd))
          (%add-event event-base event current-entry)
          (unless (monitor-fd (mux-of event-base) current-entry)
            (%remove-event event-base event))))
    (values event)))

(defmethod add-timeout ((event-base event-base) function timeout
                        &key persistent)
  (assert timeout)
  (%add-event event-base
              (make-event nil :timeout function persistent
                          (abs-timeout timeout) (normalize-timeout timeout))))

(defun calc-possible-edge-change-when-removing (fd-entry event-type)
  (cond ((and (eql event-type :read)
              (not (queue-empty-p (fd-entry-read-events fd-entry))))
         :read-del)
        ((and (eql event-type :write)
              (not (queue-empty-p (fd-entry-write-events fd-entry))))
         :write-del)))

(defmethod remove-event ((event-base event-base) event)
  (check-type (event-type event) event-type)
  (let* ((fd (event-fd event))
         (current-entry (fd-entry-of event-base fd))
         (edge-change nil))
    (if current-entry
        (progn
          (setf edge-change (calc-possible-edge-change-when-removing
                             current-entry (event-type event)))
          (%remove-event event-base event)
          (if (fd-entry-empty-p current-entry)
              (unmonitor-fd (mux-of event-base) current-entry)
              (when edge-change
                (setf (fd-entry-edge-change current-entry) edge-change)
                (update-fd (mux-of event-base) current-entry)
                (setf (fd-entry-edge-change current-entry) nil))))
        (%remove-event event-base event)))
  (values event-base))

(defmacro with-fd-handler ((event-base fd event-type function &optional timeout)
                           &body body)
  ""
  (once-only (event-base)
    (with-unique-names (event)
      `(let (,event)
         (unwind-protect
              (progn
                (setf ,event (add-fd ,event-base ,fd ,event-type ,function
                                     :timeout ,timeout))
                ,@body)
           (when ,event
             (remove-event ,event-base ,event)))))))

(defmethod event-dispatch :around ((event-base event-base)
                                   &key timeout one-shot)
  (setf (exit-p event-base) nil)
  (when timeout
    (exit-event-loop event-base :delay timeout))
  (call-next-method event-base :one-shot one-shot))

(defun recalculate-timeouts (timeouts)
  (let ((now (osicat:get-monotonic-time)))
    (dolist (ev (queue-head timeouts))
      (event-recalc-abs-timeout ev now))))

(defun dispatch-timeouts (dispatch-list)
  (dolist (ev dispatch-list)
    (funcall (event-handler ev) nil :timeout)))

(defmethod remove-events ((event-base event-base) event-list)
  (dolist (ev event-list)
    (remove-event event-base ev)))

(defvar *maximum-event-loop-timeout* 1)

(defmethod event-dispatch ((event-base event-base) &key one-shot)
  (with-accessors ((mux mux-of) (fds fds-of)
                   (exit-p exit-p) (exit-when-empty exit-when-empty-p)
                   (timeouts timeouts-of)) event-base
    (flet ((recalc-poll-timeout ()
             (calc-min-timeout (events-calc-min-rel-timeout timeouts)
                               *maximum-event-loop-timeout*)))
      (do ((poll-timeout (recalc-poll-timeout) (recalc-poll-timeout))
           (deletion-list () ())
           (dispatch-list () ()))
          ((or exit-p (and exit-when-empty (event-base-empty-p event-base))))
        (recalculate-timeouts timeouts)
        (when (and (dispatch-fd-events-once event-base poll-timeout) one-shot)
          (setf exit-p t))
        (setf (values deletion-list dispatch-list)
              (filter-expired-events
               (expired-events timeouts (osicat:get-monotonic-time))))
        (when (and dispatch-list one-shot)
          (setf exit-p t))
        (dispatch-timeouts dispatch-list)
        (remove-events event-base deletion-list)
        (queue-sort timeouts #'< #'event-abs-timeout)))))

;;; Waits for events and dispatches them.  Returns T if some events
;;; have been received, NIL otherwise.
(defun dispatch-fd-events-once (event-base timeout)
  (with-accessors ((mux mux-of) (fds fds-of) (timeouts timeouts-of))
      event-base
    (let ((deletion-list ())
          (fd-events (harvest-events mux timeout)))
      (dolist (ev fd-events)
        (destructuring-bind (fd ev-types) ev
          (let ((fd-entry (fd-entry-of event-base fd)))
            (if fd-entry
                (let ((errorp (member :error ev-types)))
                  (when errorp
                    (dispatch-error-events fd-entry)
                    (nconcf deletion-list
                            (fd-entry-all-events fd-entry)))
                  (when (member :read ev-types)
                    (dispatch-read-events fd-entry)
                    (or errorp
                        (nconcf deletion-list
                                (fd-entry-one-shot-events fd-entry :read))))
                  (when (member :write ev-types)
                    (dispatch-write-events fd-entry)
                    (or errorp
                        (nconcf deletion-list
                                (fd-entry-one-shot-events fd-entry :write)))))
                (warn "Got spurious event for non-monitored FD: ~A" fd)))))
      (dolist (ev deletion-list)
        (remove-event event-base ev))
      (consp fd-events))))

(defun expired-events (queue now)
  (queue-filter queue
                #'(lambda (to) (and to (<= to now)))
                #'event-abs-timeout))

(defun filter-expired-events (events)
  (let ((deletion-list ())
        (dispatch-list ()))
    (dolist (ev events)
      (push ev dispatch-list)
      (unless (event-persistent-p ev)
        (push ev deletion-list)))
    (values deletion-list dispatch-list)))

(defun events-calc-min-rel-timeout (timeouts)
  (let* ((now (osicat:get-monotonic-time))
         (first-valid-event (find-if #'(lambda (to)
                                         (or (null to) (< now to)))
                                     (queue-head timeouts)
                                     :key #'event-abs-timeout)))
    (when (and first-valid-event
               (event-abs-timeout first-valid-event))
      (- (event-abs-timeout first-valid-event) now))))

(defun dispatch-error-events (fd-entry)
  (dolist (ev (queue-head (fd-entry-error-events fd-entry)))
    (funcall (event-handler ev) (fd-entry-fd fd-entry) :error)))

(defun dispatch-read-events (fd-entry)
  (dolist (ev (queue-head (fd-entry-read-events fd-entry)))
    (funcall (event-handler ev) (fd-entry-fd fd-entry) :read)))

(defun dispatch-write-events (fd-entry)
  (dolist (ev (queue-head (fd-entry-write-events fd-entry)))
    (funcall (event-handler ev) (fd-entry-fd fd-entry) :write)))
