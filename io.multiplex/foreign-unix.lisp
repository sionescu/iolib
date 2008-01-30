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

;;;; sys/select.h

;;; FIXME: too low level?
(defsyscall "select" :int
  "Scan for I/O activity on multiple file descriptors."
  (nfds      :int)
  (readfds   :pointer)
  (writefds  :pointer)
  (exceptfds :pointer)
  (timeout   :pointer))

;;; FIXME: document these.
(declaim (inline fd-zero fd-clr fd-set fd-isset))

(defun fd-zero (fd-set)
  (bzero fd-set size-of-fd-set)
  (values fd-set))

(defun copy-fd-set (from to)
  (nix:memcpy to from size-of-fd-set)
  (values to))

(deftype select-file-descriptor ()
  `(mod #.fd-setsize))

(defun fd-isset (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (logbitp bit-off oldval))))

(defun fd-clr (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logandc2 oldval (ash 1 bit-off)))))
  (values fd-set))

(defun fd-set (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logior oldval (ash 1 bit-off)))))
  (values fd-set))

;;;; sys/poll.h

(defsyscall "poll" :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds)
  (timeout :int))
