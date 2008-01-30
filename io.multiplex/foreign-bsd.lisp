;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; linux.lisp --- Linux-specific definitions.
;;;
;;; Copyright (C) 2005-2006, Matthew Backes  <lucca@accela.net>
;;; Copyright (C) 2005-2006, Dan Knapp  <dankna@accela.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :io.multiplex)

;;;; sys/event.h

(defsyscall "kqueue" :int
  "Open a kernel event queue.")

(defsyscall "kevent" :int
  "Control interface for a kernel event queue."
  (kq         :int)
  (changelist :pointer) ; const struct kevent *
  (nchanges   :int)
  (eventlist  :pointer) ; struct kevent *
  (nevents    :int)
  (timeout    :pointer)) ; const struct timespec *

(defun ev-set (%kev %ident %filter %flags %fflags %data %udata)
  (with-foreign-slots ((ident filter flags fflags data udata) %kev kevent)
    (setf ident %ident filter %filter flags %flags
          fflags %fflags data %data udata %udata)))
