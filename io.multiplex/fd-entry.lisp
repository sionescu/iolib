;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; fd-entry.lisp --- FD event structure.
;;;
;;; Copyright (C) 2003 Zach Beane <xach@xach.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge,publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
  ;; TODO KLUDGE (member t), delme
  (read-event  nil :type (or null (member t) fd-event))
  (write-event nil :type (or null (member t) fd-event))
  (error-event nil :type (or null (member t) fd-event)))

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
  (not (or (fd-entry-read-event  fd-entry)
           (fd-entry-write-event fd-entry)
           (fd-entry-error-event fd-entry))))

(defun fd-entry-all-events (fd-entry)
  (remove-if #'null
             (list (fd-entry-read-event  fd-entry)
                   (fd-entry-write-event fd-entry)
                   (fd-entry-error-event fd-entry))))
