;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Main event loop.
;;;

(in-package :io.multiplex)


;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass event-base ()
  ((mux :reader mux-of)
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
  (:default-initargs :mux *default-multiplexer*
                     :exit-when-empty nil))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((base event-base) stream)
  (print-unreadable-object (base stream :type nil :identity t)
    (if (fds-of base)
        (format stream "event base, ~A FDs monitored, using: ~A"
                (hash-table-count (fds-of base)) (mux-of base))
        (format stream "event base, closed"))))


;;;-------------------------------------------------------------------------
;;; Generic functions
;;;-------------------------------------------------------------------------

(defgeneric add-fd (base fd event-type function &key timeout one-shot))

(defgeneric add-timer (event-base function timeout &key one-shot))

(defgeneric remove-event (event-base event))

(defgeneric remove-fd (event-base fd))

(defgeneric event-dispatch (event-base &key one-shot timeout &allow-other-keys))

(defgeneric exit-event-loop (event-base &key delay))

(defgeneric event-base-empty-p (event-base))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod initialize-instance :after
    ((base event-base) &key mux)
  (setf (slot-value base 'mux) (make-instance mux)))


;;;-------------------------------------------------------------------------
;;; CLOSE
;;;-------------------------------------------------------------------------

;;; KLUDGE: CLOSE is for streams. --luis
;;;
;;; Also, we might want to close FDs here.  Or have a version/argument
;;; that handles that.  Or... add finalizers to the fd streams.
(defmethod close ((event-base event-base) &key abort)
  (declare (ignore abort))
  (close-multiplexer (mux-of event-base))
  (dolist (slot '(mux fds timers fd-timers expired-events))
    (setf (slot-value event-base slot) nil))
  (values event-base))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

(defmacro with-event-base ((var &rest initargs) &body body)
  "Binds VAR to a new EVENT-BASE, instantiated with INITARGS,
within the extent of BODY.  Closes VAR."
  `(let ((,var (make-instance 'event-base ,@initargs)))
     (unwind-protect
          (locally ,@body)
       (when ,var (close ,var)))))


;;;-------------------------------------------------------------------------
;;; Utilities
;;;-------------------------------------------------------------------------

(defun fd-entry-of (event-base fd)
  (gethash fd (fds-of event-base)))

(defun (setf fd-entry-of) (fd-entry event-base fd)
  (setf (gethash fd (fds-of event-base)) fd-entry))

(defun remove-fd-entry (event-base fd)
  (remhash fd (fds-of event-base)))

(defmethod exit-event-loop ((event-base event-base) &key (delay 0))
  (add-timer event-base
             (lambda () (setf (exit-p event-base) t))
             delay :one-shot t))

(defun fd-monitored-p (event-base fd event-type)
  (let ((entry (fd-entry-of event-base fd)))
    (and entry (fd-entry-event entry event-type))))

(defmethod event-base-empty-p ((event-base event-base))
  (and (zerop (hash-table-count (fds-of event-base)))
       (priority-queue-empty-p (timers-of event-base))))


;;;-------------------------------------------------------------------------
;;; ADD-FD
;;;-------------------------------------------------------------------------

(defun expire-event (event-base event)
  (push event (expired-events-of event-base)))

(defun %add-fd-timer (event-base event timeout)
  (let ((timer (make-timer (lambda () (expire-event event-base event))
                           timeout)))
    (setf (fd-event-timer event) timer)
    (schedule-timer (fd-timers-of event-base) timer)))

(defun %add-fd (event-base fd event fd-entry timeout)
  (when timeout
    (%add-fd-timer event-base event timeout))
  (setf (fd-entry-event fd-entry (fd-event-type event)) event)
  (setf (fd-entry-of event-base fd) fd-entry)
  (values event))

(defmethod add-fd :before
    ((event-base event-base) fd event-type function &key timeout one-shot)
  (declare (ignore timeout))
  (check-type fd unsigned-byte)
  (check-type event-type fd-event-type)
  (check-type function (or symbol function))
  ;; FIXME: check the type of the timeout
  (check-type one-shot boolean)
  (when (fd-monitored-p event-base fd event-type)
    (error "FD ~A is already monitored for event ~A" fd event-type)))

(defmethod add-fd
    ((event-base event-base) fd event-type function &key timeout one-shot)
  ;; error events are forever
  (when (eql :error event-type)
    (setf timeout nil one-shot nil))
  (let ((current-fd-entry (fd-entry-of event-base fd))
        (event (make-event fd event-type function one-shot)))
    (cond
      (current-fd-entry
       (%add-fd event-base fd event current-fd-entry timeout)
       (update-fd (mux-of event-base) current-fd-entry event-type :add))
      (t
       (let ((new-fd-entry (make-fd-entry fd)))
         (%add-fd event-base fd event new-fd-entry timeout)
         (monitor-fd (mux-of event-base) new-fd-entry))))
    (values event)))


;;;-------------------------------------------------------------------------
;;; ADD-TIMER
;;;-------------------------------------------------------------------------

(defun %add-timer (event-base timer)
  (schedule-timer (timers-of event-base) timer))

(defmethod add-timer :before
    ((event-base event-base) function timeout &key one-shot)
  (check-type function (or symbol function))
  ;; FIXME: check the type of the timeout
  (check-type one-shot boolean))

(defmethod add-timer
    ((event-base event-base) function timeout &key one-shot)
  (%add-timer event-base (make-timer function timeout :one-shot one-shot)))


;;;-------------------------------------------------------------------------
;;; REMOVE-FD and REMOVE-EVENT
;;;-------------------------------------------------------------------------

(defun %remove-fd-event (event-base event)
  (let* ((fd (fd-event-fd event))
         (current-entry (fd-entry-of event-base fd)))
    (when current-entry
      (setf (fd-entry-event current-entry (fd-event-type event)) nil)
      (when-let (timer (fd-event-timer event))
        (unschedule-timer (fd-timers-of event-base) timer))
      (when (fd-entry-empty-p current-entry)
        (remove-fd-entry event-base fd))
      (if (fd-entry-empty-p current-entry)
          (unmonitor-fd (mux-of event-base) current-entry)
          (update-fd (mux-of event-base) current-entry
                     (fd-event-type event) :del)))))

(defun %remove-timer (event-base timer)
  (unschedule-timer (timers-of event-base) timer))

(defmethod remove-event ((event-base event-base) event)
  (etypecase event
    (fd-event (%remove-fd-event event-base event))
    (timer    (%remove-timer event-base event)))
  (values event-base))

(defmethod remove-fd ((event-base event-base) fd)
  (let ((entry (fd-entry-of event-base fd)))
    (cond
      (entry
       (when-let (rev (fd-entry-read-event entry))
         (%remove-fd-event event-base rev))
       (when-let (wev (fd-entry-write-event entry))
         (%remove-fd-event event-base wev))
       (assert (null (fd-entry-of event-base fd)))
       (unmonitor-fd (mux-of event-base) entry))
      (t
       (error "Trying to remove a non-monitored FD.")))))


;;;-------------------------------------------------------------------------
;;; EVENT-DISPATCH
;;;-------------------------------------------------------------------------

(defvar *minimum-event-loop-timeout* 0.5d0)
(defvar *maximum-event-loop-timeout* 1.0d0)

(defmethod event-dispatch :around ((event-base event-base)
                                   &key timeout one-shot)
  (declare (ignore one-shot))
  (setf (exit-p event-base) nil)
  (when timeout
    (exit-event-loop event-base :delay timeout))
  (call-next-method))

(defun remove-events (event-base event-list)
  (dolist (ev event-list)
    (remove-event event-base ev)))

(defmethod event-dispatch ((event-base event-base) &key one-shot timeout
                           (max-timeout *maximum-event-loop-timeout*))
  (declare (ignore timeout))
  (with-accessors ((mux mux-of) (fds fds-of) (exit-p exit-p)
                   (exit-when-empty exit-when-empty-p)
                   (timers timers-of) (fd-timers fd-timers-of)
                   (expired-events expired-events-of))
      event-base
    (flet ((poll-timeout ()
             (clamp-timeout (min-timeout (time-to-next-timer timers)
                                         (time-to-next-timer fd-timers))
                            *minimum-event-loop-timeout*
                            *maximum-event-loop-timeout*)))
      (do ((deletion-list () ())
           (eventsp nil nil)
           (poll-timeout (poll-timeout) (poll-timeout))
           (now (osicat-sys:get-monotonic-time)
                (osicat-sys:get-monotonic-time)))
          ((or exit-p (and exit-when-empty (event-base-empty-p event-base))))
        (setf expired-events nil)
        (setf (values eventsp deletion-list)
              (dispatch-fd-events-once event-base poll-timeout now))
        (remove-events event-base deletion-list)
        (when (expire-pending-timers fd-timers now) (setf eventsp t))
        (dispatch-fd-timeouts expired-events)
        (when (expire-pending-timers timers now) (setf eventsp t))
        (when (and eventsp one-shot) (setf exit-p t))))))

;;; Waits for events and dispatches them.  Returns T if some events
;;; have been received, NIL otherwise.
(defun dispatch-fd-events-once (event-base timeout now)
  (loop
     :with dlist := nil
     :with fd-events := (harvest-events (mux-of event-base) timeout)
     :for ev :in fd-events
     :do (setf dlist (%handle-one-fd event-base ev now dlist))
     :finally
     (priority-queue-reorder (fd-timers-of event-base))
     (return (values (consp fd-events) dlist))))

(defun %handle-one-fd (event-base event now deletion-list)
  (destructuring-bind (fd ev-types) event
    (let* ((readp nil) (writep nil)
           (fd-entry (fd-entry-of event-base fd))
           (errorp (and fd-entry (member :error ev-types))))
      (cond
        (fd-entry
         (when (member :read ev-types)
           (setf readp (%dispatch-event fd-entry :read
                                        (if errorp :error nil) now)))
         (when (member :write ev-types)
           (setf writep (%dispatch-event fd-entry :write
                                         (if errorp :error nil) now)))
         (when errorp
           (%dispatch-event fd-entry :error :error now)
           (setf readp t writep t))
         (when readp (push (fd-entry-read-event fd-entry) deletion-list))
         (when writep (push (fd-entry-write-event fd-entry) deletion-list)))
        (t
         (error "Got spurious event for non-monitored FD: ~A" fd)))
      (values deletion-list))))

(defun %dispatch-event (fd-entry event-type errorp now)
  (let ((ev (fd-entry-event fd-entry event-type)))
    (funcall (fd-event-handler ev)
             (fd-entry-fd fd-entry)
             event-type
             (if errorp :error nil))
    (when-let (timer (fd-event-timer ev))
      (reschedule-timer-relative-to-now timer now))
    (fd-event-one-shot-p ev)))

(defun dispatch-fd-timeouts (events)
  (dolist (ev events)
    (funcall (fd-event-handler ev)
             (fd-event-fd ev)
             (fd-event-type ev)
             :timeout)))
