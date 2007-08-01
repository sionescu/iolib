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

;;;; Monotonic Time

#-(or darwin windows)
(defun get-monotonic-time ()
  (multiple-value-bind (sec nsec)
      (nix:clock-gettime nix:clock-monotonic)
    (+ sec (/ nsec 1d9))))

;;; No sure if GetTickCount() has all of the desired properties.
;;; Also, it'd be better to use GetTickCount64() but that's
;;; Vista-only.  So, FIXME: need to check for overflow.
#+windows
(progn
  (load-foreign-library "Kernel32.dll")

  (defctype bool (:boolean :int))
  (defctype large-integer :int64)

  (defcfun ("QueryPerformanceCounter" query-perf-counter :cconv :stdcall)
      bool
    (count :pointer))

  (defun get-monotonic-time ()
    (with-foreign-object (ptr 'large-integer)
      (assert (query-perf-counter ptr))
      (mem-ref ptr 'large-integer))))

#+darwin
(progn
  (defctype mach-kern-return :int)
  (defctype mach-clock-res :int)
  (defctype mach-clock-id :int)
  (defctype mach-port :unsigned-int) ; not sure
  (defctype mach-clock-serv mach-port)

  (defconstant +mach-kern-success+ 0)
  (defconstant +mach-system-clock+ 0)

  (defcstruct mach-timespec
    (tv-sec :unsigned-int)
    (tv-nsec mach-clock-res))

  (defcfun "mach_host_self" mach-port)

  (defcfun "host_get_clock_service" mach-kern-return
    (host mach-port)
    (id mach-clock-id)
    (clock-name (:pointer mach-clock-serv)))

  (defcfun "clock_get_time" mach-kern-return
    (clock-serv mach-clock-serv)
    (cur-time mach-timespec))

  (defun get-monotonic-time ()
    (with-foreign-object (clock 'mach-clock-serv)
      (host-get-clock-service (mach-host-self) +mach-system-clock+ clock)
      (with-foreign-object (time 'mach-timespec)
        (clock-get-time (mem-ref clock :int) time)
        (with-foreign-slots ((tv-sec tv-nsec) time mach-timespec)
          (+ tv-sec (/ tv-nsec 1d9)))))))

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
  (when timeout
    (assert (not (minusp timeout)))
    (etypecase timeout
      ((or integer real) (coerce timeout 'double-float)))))

(defun abs-timeout (timeout)
  (when timeout
    (+ (get-monotonic-time) (normalize-timeout timeout))))

(defun calc-min-timeout (t1 t2)
  (if t1
      (if t2
          (min t1 t2)
          t1)
      t2))
