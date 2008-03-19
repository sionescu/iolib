;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; multiplexer.lisp --- Base class for multiplexers.
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

(defvar *available-multiplexers* nil
  "An alist of (PRIORITY . MULTIPLEXER). Smaller values mean higher priority.")

(defvar *default-multiplexer* nil
  "The default multiplexer for the current OS.")

(defun get-fd-limit ()
  "Return the maximum number of FDs available for the current process."
  (let ((fd-limit (nix:getrlimit nix:rlimit-nofile)))
    (unless (eql fd-limit nix:rlim-infinity)
      (1- fd-limit))))

(defclass multiplexer ()
  ((fd :reader fd-of)
   (fd-limit :initform (get-fd-limit)
             :initarg :fd-limit
             :reader fd-limit-of)
   (closedp :accessor multiplexer-closedp
            :initform nil))
  (:documentation "Base class for I/O multiplexers."))

(defgeneric close-multiplexer (mux)
  (:method-combination progn :most-specific-last)
  (:documentation "Close multiplexer MUX, calling close() on the multiplexer's FD if bound."))

(defgeneric monitor-fd (mux fd-entry)
  (:documentation "Add the descriptor reppresented by FD-ENTRY to multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric update-fd (mux fd-entry event-type edge-change)
  (:documentation "Update the status of the descriptor reppresented by FD-ENTRY in multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric unmonitor-fd (mux fd-entry)
  (:documentation "Remove the descriptor reppresented by FD-ENTRY from multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric harvest-events (mux timeout)
  (:documentation "Wait for events on multiplexer MUX for a maximum time of TIMEOUT seconds.
Returns a list of fd/result pairs which have one of these forms:
  (fd (:read))
  (fd (:write))
  (fd (:read :write))
  (fd . :error)"))

(defmethod close-multiplexer :around ((mux multiplexer))
  (unless (multiplexer-closedp mux)
    (call-next-method)
    (setf (multiplexer-closedp mux) t)))

(defmethod close-multiplexer progn ((mux multiplexer))
  (when (and (slot-boundp mux 'fd) (not (null (fd-of mux))))
    (nix:close (fd-of mux))
    (setf (slot-value mux 'fd) nil))
  (values mux))

(defmethod monitor-fd :around ((mux multiplexer) fd-entry)
  (if (ignore-and-print-errors (call-next-method))
      t
      (warn "FD monitoring failed for FD ~A."
            (fd-entry-fd fd-entry))))

(defmethod update-fd :around ((mux multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
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
     (pushnew (cons ,priority ',name) *available-multiplexers*
              :test #'equal)))
