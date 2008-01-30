;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; fd-wait.lisp --- Wait for events on single FDs.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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
(define-constant pollrdhup 0)

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

(define-condition poll-timeout (condition)
  ((fd :initarg :fd :reader poll-timeout-fd)
   (event-type :initarg :event-type :reader poll-timeout-event-type))
  (:report (lambda (condition stream)
             (format stream "Timeout occurred while polling file descriptor ~A for event ~S"
                     (poll-timeout-fd condition)
                     (poll-timeout-event-type condition))))
  (:documentation
   "Signaled when a timeout occurs while polling for I/O readiness
of a file descriptor."))

(defun compute-poll-flags (type)
  (ecase type
    (:read (logior pollin pollrdhup pollpri))
    (:write (logior pollout pollhup))
    (:read-write (logior pollin pollrdhup pollpri pollout pollhup))))

(defun process-poll-revents (revents fd)
  (let ((readp nil) (writep nil))
    (flags-case revents
      ((pollin pollrdhup pollpri)
       (setf readp t))
      ((pollout pollhup) (setf writep t))
      ((pollerr) (error 'poll-error :fd fd))
      ((pollnval) (error 'poll-error :fd fd
                         :identifier "Invalid file descriptor")))
    (values readp writep)))

(defun wait-until-fd-ready (file-descriptor event-type &optional timeout errorp)
  "Poll file descriptor `FILE-DESCRIPTOR' for I/O readiness. `EVENT-TYPE' must be
:READ, :WRITE or :READ-WRITE which means \"either :READ or :WRITE\".
`TIMEOUT' must be either a non-negative integer measured in seconds,
or `NIL' meaning no timeout at all. If `ERRORP' is not NIL and a timeout
occurs, then a condition of type `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readbility and writability of `FILE-DESCRIPTOR'."
  (flet ((poll-error (unix-err)
           (error 'poll-error :fd file-descriptor
                  :identifier (osicat-sys:system-error-identifier unix-err))))
    (with-foreign-object (pollfd 'pollfd)
      (bzero pollfd size-of-pollfd)
      (with-foreign-slots ((fd events revents) pollfd pollfd)
        (setf fd file-descriptor
              events (compute-poll-flags event-type))
        (handler-case
            (let ((ret (nix:repeat-upon-condition-decreasing-timeout
                           ((nix:eintr) tmp-timeout timeout)
                         (poll pollfd 1 (timeout->milisec timeout)))))
              (when (zerop ret)
                (if errorp
                    (error 'poll-timeout :fd file-descriptor :event-type event-type)
                    (return-from wait-until-fd-ready (values nil nil)))))
          (nix:posix-error (err) (poll-error err)))
        (process-poll-revents revents file-descriptor)))))

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
