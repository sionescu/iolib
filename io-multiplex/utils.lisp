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

(defun timeout->timeval (timeout tv)
  (with-foreign-slots ((et:sec et:usec) tv et:timeval)
    (multiple-value-bind (sec usec) (decode-timeout timeout)
     (setf et:sec  sec
           et:usec usec))))

(defun timeout->timespec (timeout ts)
  (with-foreign-slots ((et:sec et:nsec) ts et:timespec)
    (multiple-value-bind (sec usec) (decode-timeout timeout)
      (setf et:sec sec
            et:nsec (* 1000 usec)))))

(defun timeout->milisec (timeout)
  (if timeout
      (multiple-value-bind (sec usec) (decode-timeout timeout)
        (+ (* sec 1000)
           (truncate usec 1000)))
      -1))

(defmacro flags-case (mask &body clauses)
  (once-only (mask)
    `(progn ,@(loop :for clause :in clauses
                 :collect `(when (logtest ,(let ((flags (first clause)))
                                                (if (listp flags)
                                                    `(logior ,@flags)
                                                    flags))
                                          ,mask)
                             ,(second clause))))))

(defmacro ignore-and-print-errors (&body body)
  `(handler-case (progn ,@body)
     (error (error)
       (warn "Caught a ~A: ~A, ignoring it."
             (type-of error) error))))
