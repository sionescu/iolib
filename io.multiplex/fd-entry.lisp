;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; fd-entry.lisp --- FD event structure.
;;;

(in-package :io.multiplex)

;;;; EVENT

(deftype fd-event-type ()
  '(member :read :write :error))

(defstruct (fd-event (:constructor make-event (fd type handler one-shot-p))
                     (:copier nil))
  (fd nil :type unsigned-byte)
  (type nil :type fd-event-type)
  (handler nil :type function)
  (timer nil :type (or null timer))
  ;; if an event is not persistent it is removed
  ;; after it is triggered or if it times out
  (one-shot-p nil :type boolean))

;;;; FD-ENTRY

(defstruct (fd-entry (:constructor make-fd-entry (fd))
                     (:copier nil))
  (fd 0 :type unsigned-byte)
  (read-event  nil :type (or null fd-event))
  (write-event nil :type (or null fd-event))
  (error-event nil :type (or null fd-event)))

(defun fd-entry-event (fd-entry event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type fd-event-type)
  (case event-type
    (:read  (fd-entry-read-event  fd-entry))
    (:write (fd-entry-write-event fd-entry))
    (:error (fd-entry-error-event fd-entry))))

(defun (setf fd-entry-event) (event fd-entry event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type fd-event-type)
  (case event-type
    (:read  (setf (fd-entry-read-event  fd-entry) event))
    (:write (setf (fd-entry-write-event fd-entry) event))
    (:error (setf (fd-entry-error-event fd-entry) event))))

(defun fd-entry-empty-p (fd-entry)
  (or (null (fd-entry-read-event  fd-entry))
      (null (fd-entry-write-event fd-entry))))
