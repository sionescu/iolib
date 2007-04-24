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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *available-multiplexers* nil)
  (defvar *best-available-multiplexer* nil))

;;;
;;; Event-Base
;;;
(defclass event-base ()
  ((mux :initform (make-instance *best-available-multiplexer*)
        :reader mux-of)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds-of)
   (timeouts :initform (make-queue)
             :reader timeouts-of)
   (main-timeout :initform nil
                 :accessor main-timeout-of)
   (exit :initform nil
         :accessor exit-p)))


(defmethod print-object ((base event-base) stream)
  (print-unreadable-object (base stream :type nil :identity t)
    (format stream "event base, ~A FDs monitored, using: ~A"
            (hash-table-count (fds-of base))
            (mux-of base))))


(defgeneric close-event-base (event-base)
  (:method ((event-base event-base))
    (with-accessors ((mux mux-of)) event-base
      (close-multiplexer mux)
      (mapc #'(lambda (slot)
                (setf (slot-value event-base slot) nil))
            '(fds timeouts exit))
      event-base)))


(defgeneric add-fd (base fd event-type function &key timeout persistent))


(defgeneric add-timeout (event-base function timeout &key persistent))


(defgeneric remove-event (event-base event))


(defgeneric remove-events (event-base event-list))


(defgeneric event-dispatch (event-base &key timeout only-once))


(defgeneric exit-event-loop (event-base &key delay)
  (:method ((event-base event-base) &key delay)
    (setf (main-timeout-of event-base)
          (add-timeout event-base
                       #'(lambda (fd event-type)
                           (declare (ignore fd event-type))
                           (setf (exit-p event-base) t))
                       delay :persistent nil))))


(defgeneric event-base-empty-p (event-base)
  (:method ((event-base event-base))
    (and (zerop (hash-table-count (fds-of event-base)))
         (queue-empty-p (timeouts-of event-base)))))


(defgeneric fd-entry-of (event-base fd)
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


(defmethod add-fd ((event-base event-base) fd event-type function &key timeout persistent)
  (check-type event-type fd-event)

  (let ((fd-limit (fd-limit-of (mux-of event-base))))
    (when (and fd-limit (> fd fd-limit))
      (error "Cannot add such a large FD: ~A" fd)))
  (let ((current-entry (fd-entry-of event-base fd))
        (event (make-event fd event-type function persistent
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


(defmethod add-timeout ((event-base event-base) function timeout &key persistent)
  (assert timeout)
  (%add-event event-base (make-event nil :timeout function persistent
                                     (abs-timeout timeout)
                                     (normalize-timeout timeout))))


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


(defmethod remove-events ((event-base event-base) event-list)
  (dolist (ev event-list)
    (remove-event event-base ev)))


(defmacro with-fd-handler ((event-base fd event-type function
                            &optional timeout)
                           &body body)
  (let ((event (gensym "EVENT-")))
    `(let (,event)
       (unwind-protect
            (progn
              (setf ,event (add-fd ,event-base ,fd ,event-type ,function
                                   :persistent t
                                   :timeout ,timeout))
              ,@body)
         (when ,event
           (remove-event ,event-base ,event))))))


(defmethod event-dispatch :around ((event-base event-base) &key timeout only-once)
  (setf (exit-p event-base) nil)
  (setf (main-timeout-of event-base) nil)
  (when timeout
    (exit-event-loop event-base :delay timeout)
    (setf only-once t))
  (unless (event-base-empty-p event-base)
    (call-next-method event-base :timeout timeout
                      :only-once only-once)))


(defmethod event-dispatch ((event-base event-base) &key timeout only-once)
  (with-accessors ((mux mux-of) (exit-p exit-p)
                   (fds fds-of) (timeouts timeouts-of)) event-base
    (let* ((min-event-timeout (events-calc-min-rel-timeout timeouts))
           (actual-timeout (calc-min-timeout min-event-timeout timeout))
           (before nil)
           (after nil))
      (loop
         :with deletion-list := ()
         :with dispatch-list := ()
         :do

         (setf before (et::gettime))
         (mapc #'(lambda (ev)
                   (event-recalc-abs-timeout ev before))
               (queue-head timeouts))
         (dispatch-fd-events-once event-base actual-timeout)
         (setf after (et::gettime))

         (let ((main-timeout (main-timeout-of event-base)))
           (when main-timeout
             (remove-event event-base main-timeout)
             (setf (main-timeout-of event-base) nil)))

         (setf (values deletion-list dispatch-list)
               (filter-expired-events (expired-events timeouts after)))
         (dispatch-timeouts dispatch-list)
         (remove-events event-base deletion-list)

         (queue-sort timeouts #'< #'event-abs-timeout)

         :when (or only-once
                   exit-p
                   (event-base-empty-p event-base))
         :do (loop-finish)))))


(defun dispatch-fd-events-once (event-base timeout)
  (with-accessors ((mux mux-of) (fds fds-of)
                   (timeouts timeouts-of)) event-base
    (let ((deletion-list ())
          (fd-events (harvest-events mux timeout)))
      (dolist (ev fd-events)
        (destructuring-bind (fd ev-types) ev
          (let ((fd-entry (fd-entry-of event-base fd)))
            (if fd-entry
                (progn
                  (when (member :error ev-types)
                    (dispatch-error-events fd-entry)
                    (setf deletion-list (fd-entry-all-events fd-entry)))
                  (when (member :read ev-types)
                    (dispatch-read-events fd-entry)
                    (setf deletion-list
                          (nconc deletion-list
                                 (fd-entry-one-shot-events fd-entry :read))))
                  (when (member :write ev-types)
                    (dispatch-write-events fd-entry)
                    (setf deletion-list
                          (nconc deletion-list
                                 (fd-entry-one-shot-events fd-entry :write)))))
                (warn "Got spurious event for non-monitored FD: ~A" fd)))))
      (dolist (ev deletion-list)
        (remove-event event-base ev)))))


(defun expired-events (queue now)
  (queue-filter queue
                #'(lambda (to)
                    (and to (<= to now)))
                #'event-abs-timeout))


(defun filter-expired-events (events)
  (let ((deletion-list ())
        (dispatch-list ()))
    (dolist (ev events)
      (push ev dispatch-list)
      (unless (event-persistent-p ev)          
        (push ev deletion-list)))
    (values deletion-list dispatch-list)))


(defun dispatch-timeouts (dispatch-list)
  (dolist (ev dispatch-list)
    (funcall (event-handler ev) nil :timeout)))


(defun events-calc-min-rel-timeout (timeouts)
  (let* ((now (et::gettime))
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

;;;
;;; FD-Entry
;;;
(deftype fd-event ()
  '(member :read :write :error))


(deftype event-type ()
  '(or fd-event (member :timeout)))


(defstruct (fd-entry
             (:constructor make-fd-entry (fd))
             (:copier nil))
  (fd 0 :type unsigned-byte)
  (edge-change nil :type symbol)
  (read-events  (make-queue) :type queue)
  (write-events (make-queue) :type queue)
  (error-events (make-queue) :type queue))


(defun fd-entry-event-list (fd-entry event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type fd-event)
  (case event-type
    (:read (fd-entry-read-events fd-entry))
    (:write (fd-entry-write-events fd-entry))
    (:error (fd-entry-error-events fd-entry))))


(defun (setf fd-entry-event-list) (fd-entry event-list event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type fd-event)
  (case event-type
    (:read (setf (fd-entry-read-events fd-entry) event-list))
    (:write (setf (fd-entry-write-events fd-entry) event-list))
    (:error (setf (fd-entry-error-events fd-entry) event-list))))


(defun fd-entry-empty-p (fd-entry)
  (and (queue-empty-p (fd-entry-read-events fd-entry))
       (queue-empty-p (fd-entry-write-events fd-entry))
       (queue-empty-p (fd-entry-error-events fd-entry))))


(defun fd-entry-add-event (fd-entry event)
  (queue-enqueue (fd-entry-event-list fd-entry (event-type event))
                 event))


(defun fd-entry-del-event (fd-entry event)
  (queue-delete (fd-entry-event-list fd-entry (event-type event))
                event))


(defun fd-entry-all-events (fd-entry)
  (append (queue-head (fd-entry-read-events fd-entry))
          (queue-head (fd-entry-write-events fd-entry))
          (queue-head (fd-entry-error-events fd-entry))))


(defun fd-entry-one-shot-events (fd-entry event-type)
  (remove-if #'event-persistent-p
             (queue-head (fd-entry-event-list fd-entry event-type))))

;;;
;;; Event
;;;
(defstruct (event
             (:constructor make-event (fd type handler persistent-p
                                       abs-timeout timeout))
             (:copier nil))
  ;; a file descriptor or nil in case of a timeout
  (fd nil :type (or null unsigned-byte))
  (type nil :type (or null event-type))
  (handler nil :type (or null function))
  ;; if an event is not persistent it is removed
  ;; after it occurs or if it times out
  (persistent-p nil :type boolean)
  (abs-timeout nil :type (or null timeout))
  (timeout nil :type (or null timeout)))


(defun event-timed-out-p (event timeout)
  (let ((ev-to (event-abs-timeout event)))
    (when (and ev-to timeout)
      (< timeout ev-to))))


(defun event-recalc-abs-timeout (event now)
  (setf (event-abs-timeout event)
        (+ now (event-timeout event))))

;;;
;;; Multiplexer
;;;

(defun get-fd-limit ()
  (let ((fd-limit (et:get-resource-limit et:rlimit-nofile)))
    (unless (eql fd-limit et:rlim-infinity)
      (1- fd-limit))))


(defclass multiplexer ()
  ((fd :reader fd-of)
   (fd-limit :initform (get-fd-limit)
             :initarg :fd-limit
             :reader fd-limit-of)))


(defgeneric monitor-fd (mux fd-entry)
  (:method ((mux multiplexer) fd-entry)
    (declare (ignore fd-entry))
    t))


(defgeneric update-fd (mux fd-entry)
  (:method ((mux multiplexer) fd-entry)
    (declare (ignore fd-entry))
    t))


(defgeneric unmonitor-fd (mux fd-entry)
  (:method ((mux multiplexer) fd-entry)
    (declare (ignore fd-entry))
    t))


(defgeneric harvest-events (mux timeout))


(defgeneric close-multiplexer (mux)
  (:method-combination progn :most-specific-last)
  (:method progn ((mux multiplexer))
    (when (slot-value mux 'fd)
      (et:close (fd-of mux))
      (setf (slot-value mux 'fd) nil))
    mux))


(defmethod monitor-fd :around ((mux multiplexer) fd-entry)
  (if (ignore-and-print-errors (call-next-method))
      t
      (warn "FD monitoring failed for FD ~A."
            (fd-entry-fd fd-entry))))


(defmethod update-fd :around ((mux multiplexer) fd-entry)
  (if (ignore-and-print-errors (call-next-method))
      t
      (warn "FD status update failed for FD ~A."
            (fd-entry-fd fd-entry))))


(defmethod unmonitor-fd :around ((mux multiplexer) fd-entry)
  (if (ignore-and-print-errors (call-next-method))
      t
      (warn "FD unmonitoring failed for FD ~A."
            (fd-entry-fd fd-entry))))


(defmacro define-multiplexer (name priority superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (pushnew (cons ,priority ',name)
              *available-multiplexers*)))

;;;;
;;;; Misc
;;;;

;; FIXME: Until a way to autodetect platform features is implemented
(iolib-utils:define-constant et::pollrdhup 0)

(defun wait-until-fd-ready (fd event-type &optional timeout)
  ;; FIXME: this conses badly for its return value
  ;; solution: (1) use a fixnum bitmap, just like C
  ;; (2) if we really want to expose only lists of keyword as the API,
  ;; cache a bitmap-indexed vector of all the combinations (sharing tails)
  (flet ((choose-poll-flags (type)
           (ecase type
             (:read (logior et:pollin et::pollrdhup et:pollpri))
             (:write (logior et:pollout et:pollhup))
             (:read-write (logior et:pollin et::pollrdhup et:pollpri
                                  et:pollout et:pollhup)))))
    (let ((status ()))
      (with-foreign-object (pollfd 'et:pollfd)
        (et:bzero pollfd et:size-of-pollfd)
        (with-foreign-slots ((et:fd et:events et:revents) pollfd et:pollfd)
          (setf et:fd fd
                et:events (choose-poll-flags event-type))
          (handler-case
              (let ((ret (et:repeat-upon-eintr
                           (et:poll pollfd 1 (timeout->milisec timeout)))))
                (when (zerop ret)
                  (return-from wait-until-fd-ready '(:timeout))))
            (et:unix-error (err)
              (declare (ignore err))
              (return-from wait-until-fd-ready '(:error))))
          (flags-case et:revents
            ((et:pollout et:pollhup)              (push :write status))
            ((et:pollin et::pollrdhup et:pollpri) (push :read  status))
            ((et:pollerr et:pollnval)             (push :error status)))
          (return-from wait-until-fd-ready status))))))

(defun fd-ready-p (fd &optional (event-type :read))
  (not (member :timeout (wait-until-fd-ready fd event-type 0) :test #'eq)))
