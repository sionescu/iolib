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

(in-package :io.multiplex)

(defun timeout->timeval (timeout tv)
  (with-foreign-slots ((et:tv-sec et:tv-usec) tv et:timeval)
    (multiple-value-bind (sec usec) (decode-timeout timeout)
     (setf et:tv-sec  sec
           et:tv-usec usec))))


(defun timeout->timespec (timeout ts)
  (with-foreign-slots ((et:ts-sec et:ts-nsec) ts et:timespec)
    (multiple-value-bind (sec usec) (decode-timeout timeout)
      (setf et:ts-sec sec
            et:ts-nsec (* 1000 usec)))))


(defmacro flags-case (mask &body clauses)
  (let ((newm (gensym "MASK")))
    `(let ((,newm ,mask))
       (cond ,@(loop :for clause :in clauses
                  :collect `((logtest ,(let ((flags (first clause)))
                                            (if (listp flags)
                                                `(logand ,@flags)
                                                flags))
                                      ,newm)
                             ,(second clause)))))))


(defmacro ignore-and-print-errors (&body body)
  `(handler-case (progn ,@body)
     (error (error)
       (warn "Caught a ~A: ~A, ignoring it."
             (type-of error) error))))


;; (defmacro with-restart-on-eintr (&body body)
;;   `(loop :for exit-p
;;       :while (not exit-p) :do
;;       (handler-case
;;           (progn ,@body)
;;         (et:unix-error-intr (err)
;;           (declare (ignore err))
;;           (setf exit-p t)))))
