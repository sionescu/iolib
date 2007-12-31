;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; fd-wait.lisp --- Wait for events on single FDs.
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

;;; FIXME: Until a way to autodetect platform features is implemented
#+(or darwin freebsd)
(define-constant nix::pollrdhup 0)

(define-condition poll-error (error)
  ((fd :initarg :fd :reader poll-error-fd)
   (identifier :initarg :identifier :initform "Unknown error"
               :reader poll-error-identifier))
  (:report (lambda (condition stream)
             (format stream "Error caught while polling file descriptor ~A: ~A"
                     (poll-error-fd condition)
                     (poll-error-identifier condition))))
  (:documentation
   "Signaled when an error occurs while polling for I/O readiness
of a file descriptor."))

(defun compute-poll-flags (type)
  (ecase type
    (:read (logior nix::pollin nix::pollrdhup nix::pollpri))
    (:write (logior nix::pollout nix::pollhup))
    (:read-write (logior nix::pollin nix::pollrdhup nix::pollpri
                         nix::pollout nix::pollhup))))         

(defun process-poll-revents (revents fd)
  (let ((readp nil) (writep nil))
    (flags-case revents
      ((nix::pollin nix::pollrdhup nix::pollpri)
       (setf readp t))
      ((nix::pollout nix::pollhup) (setf writep t))
      ((nix::pollerr) (error 'poll-error :fd fd))
      ((nix::pollnval) (error 'poll-error :fd fd
                              :identifier "Invalid file descriptor")))
    (values readp writep)))

(defun wait-until-fd-ready (fd event-type &optional timeout)
  "Poll file descriptor `FD' for I/O readiness. `EVENT-TYPE' must be
:READ, :WRITE or :READ-WRITE which means \"either :READ or :WRITE\".
`TIMEOUT' must be either a non-negative integer measured in seconds,
or `NIL' meaning no timeout at all."
  (flet ((poll-error (unix-err)
           (error 'poll-error :fd fd
                  :identifier (osicat-sys:system-error-identifier unix-err))))
    (with-foreign-object (pollfd 'nix::pollfd)
      (nix:bzero pollfd nix::size-of-pollfd)
      (with-foreign-slots ((nix::fd nix::events nix::revents)
                           pollfd nix::pollfd)
        (setf nix::fd fd
              nix::events (compute-poll-flags event-type))
        (handler-case
            (let ((ret (nix:repeat-upon-condition-decreasing-timeout
                           ((nix:eintr) tmp-timeout timeout)
                         (nix:poll pollfd 1 (timeout->milisec timeout)))))
              (when (zerop ret)
                (return-from wait-until-fd-ready (values nil nil))))
          (nix:posix-error (err) (poll-error err)))
        (process-poll-revents nix::revents fd)))))

(defun fd-ready-p (fd &optional (event-type :read))
  "Tests file-descriptor `FD' for I/O readiness. `EVENT-TYPE'
must be :READ, :WRITE or :READ-WRITE which means \"either :READ
or :WRITE\"."
  (multiple-value-bind (readp writep)
      (wait-until-fd-ready fd event-type 0)
    (ecase event-type
      (:read readp)
      (:write writep)
      (:read-write (or readp writep)))))

(defun fd-readablep (fd)
  (nth-value 0 (wait-until-fd-ready fd :read 0)))

(defun fd-writablep (fd)
  (nth-value 1 (wait-until-fd-ready fd :write 0)))
