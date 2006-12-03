;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
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

;;;
;;; Class definitions
;;;

(defstruct (handler
             (:constructor make-handler (fd read-func write-func except-func))
             (:copier nil))
  (fd            0 :type et:select-file-descriptor)
  (read-func   nil :type (or function null))
  (write-func  nil :type (or function null))
  (except-func nil :type (or function null)))

(defclass multiplex-interface ()
  ((fd-handlers :initform (make-hash-table :test 'eql) :reader fd-handlers)
   (fd-set-size :initform 0)))

(defmethod initialize-instance :after ((interface multiplex-interface)
                                       &key size)
  (setf (slot-value interface 'fd-set-size) size))

(defgeneric fd-handler (multiplex-interface fd)
  (:method ((interface multiplex-interface) fd)
    (gethash fd (fd-handlers interface))))

(defgeneric monitor-fd (multiplex-interface handler)
  (:method-combination progn :most-specific-last))

(defgeneric modify-fd (multiplex-interface fd
                       &key read-handler write-handler except-handler)
  (:method-combination progn :most-specific-last))

(defgeneric add-fd-handlers (multiplex-interface fd
                             &key read-handler write-handler except-handler)
  (:method-combination progn :most-specific-last))

(defgeneric unmonitor-fd (multiplex-interface handler)
  (:method-combination progn :most-specific-first))

(defgeneric remove-fd-handlers (multiplex-interface fd
                                &key read write except all)
  (:method-combination progn :most-specific-first))

(defgeneric serve-fd-events (multiplex-interface &key))

(defgeneric close-multiplex-interface (multiplex-interface)
  (:method ((interface multiplex-interface))
    t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *multiplex-available-interfaces* nil)
  (defvar *multiplex-best-interface* nil))

(defmacro define-iomux-interface (name priority)
  `(pushnew (cons ,priority ',name)
            *multiplex-available-interfaces*))

;; small utility
(defun fd-open-p (fd)
  (with-foreign-object (stat 'et:stat)
    (handler-case
        (progn (et:stat fd stat) t)
      (et:unix-error-badf (err)
        (declare (ignore err))
        nil))))

(defun finalize-object-closing-fd (object fd)
  (finalize object #'(lambda () (et:close fd))))



;;;
;;; Base methods
;;;

(defmethod monitor-fd progn ((interface multiplex-interface) handler)
  (setf (gethash (handler-fd handler) (fd-handlers interface))
        handler)
  (values interface))

(defmethod modify-fd progn ((interface multiplex-interface) fd
                            &key read-handler write-handler except-handler)
  (let ((handler (fd-handler interface fd)))
    (setf (handler-read-func   handler) read-handler)
    (setf (handler-write-func  handler) write-handler)
    (setf (handler-except-func handler) except-handler))
  (values interface))

(defmethod add-fd-handlers progn ((interface multiplex-interface) fd
                                  &key read-handler write-handler except-handler)
  (assert (or read-handler write-handler except-handler))

  (let ((current-handler (fd-handler interface fd)))
    (if current-handler
        (progn
          (modify-fd interface fd
                     :read-handler   (or read-handler
                                         (handler-read-func current-handler))
                     :write-handler  (or write-handler
                                         (handler-except-func current-handler))
                     :except-handler (or except-handler
                                         (handler-except-func current-handler))))
        (progn
          (setf current-handler (make-handler fd read-handler write-handler except-handler))
          (monitor-fd interface current-handler))))
  (values interface))

(defmethod unmonitor-fd progn ((interface multiplex-interface) handler)
  (remhash (handler-fd handler) (fd-handlers interface))
  (values interface))

(defmethod remove-fd-handlers progn ((interface multiplex-interface) fd
                                     &key read write except all)
  (unless all
    (assert (or read write except)))

  (let ((current-handler (fd-handler interface fd)))
    (when current-handler
      (if all
          (unmonitor-fd interface current-handler)
          (progn
            (when read   (setf (handler-read-func   current-handler) nil))
            (when write  (setf (handler-write-func  current-handler) nil))
            (when except (setf (handler-except-func current-handler) nil))
            (if (or (handler-read-func   current-handler)
                    (handler-write-func  current-handler)
                    (handler-except-func current-handler))
                (modify-fd interface fd
                           :read-handler   (handler-read-func current-handler)
                           :write-handler  (handler-except-func current-handler)
                           :except-handler (handler-except-func current-handler))
                (unmonitor-fd interface current-handler))))))
  (values interface))

;; if there are handlers installed save them and restore them at the end
;; (defmacro with-fd-handlers ((fd &key read-handler write-handler except-handler) &body body)
;;   (let ((tmp-handler (gensym)))
;;     `(let ((,tmp-handler (gethash ,fd fd-handlers)))
;;        (unwind-protect
;;             (progn
;;               (when ,tmp-handler
;;                 (remove-fd-handlers ,fd :all t))
;;               (add-fd-handlers ,fd :read-handler ,read-handler
;;                                    :write-handler ,write-handler
;;                                    :except-handler ,except-handler)
;;               ,@body)
;;          (if ,tmp-handler
;;              (setf (gethash ,fd fd-handlers) ,tmp-handler)
;;              (remove-fd-handlers ,fd :all t))))))
