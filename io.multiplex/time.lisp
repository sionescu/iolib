;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; time.lisp --- Various time-related functions.
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

;;;; Timeouts

(deftype timeout ()
  'double-float)

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (typecase timeout
    (integer (values timeout 0))
    (null    nil)
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'double-float))
       (declare (type unsigned-byte q)
                (type double-float r))
       (values q (the (values unsigned-byte t) (truncate (* r 1d6))))))
    (t
     (error "Timeout is not a real number or NIL: ~S" timeout))))

(defun normalize-timeout (timeout)
  (assert (not (minusp timeout)) (timeout)
           "The timeout must be non-negative: ~A" timeout)
  (coerce timeout 'double-float))

(defun abs-timeout (timeout)
  (+ (osicat:get-monotonic-time) (normalize-timeout timeout)))

(defun min-timeout (&rest timeouts)
  (collect-min (choose-if #'identity (scan timeouts))))
