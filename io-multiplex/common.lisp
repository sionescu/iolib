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

;;;;
;;;; Type definitions
;;;;

(deftype event-type ()
  '(member :read :write :except :error))

;;;
;;; FD Entry
;;;
(defstruct (fd-entry
             (:constructor make-fd-entry (fd read-handlers write-handlers
                                          except-handlers error-handlers))
             (:copier nil))
  (fd 0 :type et:select-file-descriptor)
  (read-handlers   nil :type list)
  (write-handlers  nil :type list)
  (except-handlers nil :type list)
  (error-handlers  nil :type list))

(defun fd-entry-handler-list (fd-entry event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type event-type)
  (case event-type
    (:read (fd-entry-read-handlers fd-entry))
    (:write (fd-entry-write-handlers fd-entry))
    (:except (fd-entry-except-handlers fd-entry))
    (:error (fd-entry-error-handlers fd-entry))))

(defun (setf fd-entry-handler-list) (handler-list fd-entry event-type)
  (check-type fd-entry fd-entry)
  (check-type event-type event-type)
  (case event-type
    (:read (setf (fd-entry-read-handlers fd-entry) handler-list))
    (:write (setf (fd-entry-write-handlers fd-entry) handler-list))
    (:except (setf (fd-entry-except-handlers fd-entry) handler-list))
    (:error (setf (fd-entry-error-handlers fd-entry) handler-list))))

(defun fd-entry-empty-p (fd-entry)
  (not (or (fd-entry-read-handlers fd-entry)
           (fd-entry-write-handlers fd-entry)
           (fd-entry-except-handlers fd-entry)
           (fd-entry-error-handlers fd-entry))))

;;;
;;; Handler
;;;
(defstruct (handler
             (:constructor make-handler (event-type function))
             (:copier nil))
  (event-type nil :type (or null event-type))
  (function   nil :type (or null function)))

;;;
;;; Multiplexer
;;;
(defclass multiplexer ()
  ((fd-entries :initform (make-hash-table :test 'eql) :reader fd-entries)
   (fd-set-size :initform 0)))

(defmethod initialize-instance :after ((mux multiplexer)
                                       &key size)
  (setf (slot-value mux 'fd-set-size) size))

(defgeneric fd-entry (mux fd)
  (:method ((mux multiplexer) fd)
    (gethash fd (fd-entries mux))))

(defgeneric monitor-fd (mux fd-entry)
  (:method-combination progn :most-specific-last))

(defgeneric update-fd (mux fd-entry)
  (:method-combination progn :most-specific-last)
  (:method progn ((mux multiplexer) fd-entry)
    t))

(defgeneric add-fd-handler (mux fd event-type function)
  (:method-combination progn :most-specific-last))

(defgeneric unmonitor-fd (mux fd)
  (:method-combination progn :most-specific-first))

(defgeneric remove-fd-handler (mux fd handler)
  (:method-combination progn :most-specific-first))

(defgeneric serve-fd-events (mux &key timeout))

(defgeneric close-multiplexer (mux)
  (:method ((mux multiplexer))
    t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *available-multiplexers* nil)
  (defvar *best-multiplexer* nil))

(defmacro define-multiplexer (name priority superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (pushnew (cons ,priority ',name)
              *available-multiplexers*)))

(defun fd-open-p (fd)
  (with-foreign-object (stat 'et:stat)
    (handler-case
        (progn (et:stat fd stat) t)
      (et:unix-error-badf (err)
        (declare (ignore err))
        nil))))

(defun finalize-object-closing-fd (object fd)
  (finalize object #'(lambda () (et:close fd))))



;;;;
;;;; Base methods
;;;;

(defmethod monitor-fd progn ((mux multiplexer) fd-entry)
  (let ((fd (fd-entry-fd fd-entry)))
    (setf (gethash fd (fd-entries mux)) fd-entry)
    (values fd)))

(defmethod add-fd-handler progn ((mux multiplexer)
                                 fd event-type function)
  (check-type event-type event-type)

  (let ((current-entry (fd-entry mux fd))
        (handler (make-handler event-type function)))
    (if current-entry
        (push handler (fd-entry-handler-list current-entry event-type))
        (progn
          (setf current-entry (make-fd-entry fd nil nil nil nil))
          (push handler (fd-entry-handler-list current-entry event-type))
          (monitor-fd mux current-entry)))
    (values handler)))

(defmethod unmonitor-fd progn ((mux multiplexer) fd)
  (remhash fd (fd-entries mux))
  (values fd))

(defmethod remove-fd-handler progn ((mux multiplexer)
                                    fd handler)
  (check-type (handler-event-type handler) event-type)

  (let ((event-type (handler-event-type handler))
        (current-entry (fd-entry mux fd)))
    (when current-entry
      (setf (fd-entry-handler-list current-entry event-type)
            (delete handler (fd-entry-handler-list current-entry event-type) :test 'eq))
      (when (fd-entry-empty-p current-entry)
        (unmonitor-fd mux fd))))
  (values mux))

;; if there are handlers installed save them and restore them at the end
(defmacro with-fd-handler ((mux fd event-type function)
                           &body body)
  (let ((handler (gensym "HANDLER-")))
    `(let (,handler)
       (unwind-protect
            (progn
              (setf ,handler (add-fd-handler ,mux ,fd ,event-type ,function))
              ,@body)
         (when ,handler
           (remove-fd-handler ,mux ,fd ,handler))))))


;;;;
;;;; Other utilities
;;;;

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (typecase timeout
    (integer (values timeout 0))
    (null (values 0 0))
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'single-float))
       (declare (type unsigned-byte q) (single-float r))
       (values q (the (values unsigned-byte t) (truncate (* r 1f6))))))
    (t
     (error "Timeout is not a real number or NIL: ~S" timeout))))

(defun wait-until-fd-usable (mux fd event-type &optional timeout)
  (let (status)
    (flet ((callback (fd type)
             (cond ((member type '(:error :except))
                    (setf status :except))
                   ((eql type event-type)
                    (setf status :ok)))))
      (with-fd-handler (mux fd event-type #'callback)
        (loop
           (serve-fd-events mux :timeout timeout)
           (when status
             (return-from wait-until-fd-usable status)))))))
