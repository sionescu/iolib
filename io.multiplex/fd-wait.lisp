;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wait for events on single FDs.
;;;

(in-package :io.multiplex)

;;; FIXME: Until a way to autodetect platform features is implemented
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'pollrdhup)
    (defconstant pollrdhup 0)))

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
    (:input  (logior pollin pollrdhup pollpri))
    (:output (logior pollout))
    (:io     (logior pollin pollrdhup pollpri pollout))))

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
  "Poll file descriptor `FILE-DESCRIPTOR' for I/O readiness.
`EVENT-TYPE' must be either :INPUT, :OUTPUT or :IO.
`TIMEOUT' must be either a non-negative integer measured in seconds,
or `NIL' meaning no timeout at all. If `ERRORP' is not NIL and a timeout
occurs, then a condition of type `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readability and writeability of `FILE-DESCRIPTOR'."
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
                           ((nix:eintr) remaining-time timeout)
                         (poll pollfd 1 (timeout->milisec remaining-time)))))
              (when (zerop ret)
                (if errorp
                    (error 'poll-timeout :fd file-descriptor :event-type event-type)
                    (return* (values nil nil)))))
          (nix:posix-error (err) (poll-error err)))
        (process-poll-revents revents file-descriptor)))))

(defun fd-ready-p (fd &optional (event-type :input))
  "Tests file-descriptor `FD' for I/O readiness.
`EVENT-TYPE' must be either :INPUT, :OUTPUT or :IO ."
  (multiple-value-bind (readp writep)
      (wait-until-fd-ready fd event-type 0)
    (ecase event-type
      (:input  readp)
      (:output writep)
      (:io     (or readp writep)))))

(defun fd-readablep (fd)
  (nth-value 0 (wait-until-fd-ready fd :input 0)))

(defun fd-writablep (fd)
  (nth-value 1 (wait-until-fd-ready fd :output 0)))
