;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
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

(defstruct (event (:constructor make-event (fd type handler persistent-p
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

(defun event-recalc-abs-timeout (event now)
  (setf (event-abs-timeout event)
        (+ now (event-timeout event))))

;;;; FD-ENTRY

(deftype fd-event ()
  '(member :read :write :error))

(deftype event-type ()
  '(or fd-event (member :timeout)))

(defstruct (fd-entry (:constructor make-fd-entry (fd))
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
