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


(defun gettime ()
  (multiple-value-bind (sec nsec)
      (et:clock-get-time et:clock-monotonic)
    (+ sec (/ nsec 1d9))))


(defun normalize-timeout (timeout)
  (when timeout
    (assert (not (minusp timeout)))
    (etypecase timeout
      ((or integer real) (coerce timeout 'double-float)))))


(defun abs-timeout (timeout)
  (when timeout
    (+ (gettime) (normalize-timeout timeout))))


(defun calc-min-timeout (t1 t2)
  (when (and t1 (minusp t1)) (setf t1 0.0d0))
  (when (and t2 (minusp t2)) (setf t2 0.0d0))
  (if t1
      (if t2
          (min t1 t2)
          t1)
      t2))
