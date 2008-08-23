;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *NIX-specific routines.
;;;

(in-package :io.zeta-streams)

(defun compute-poll-flags (type)
  (ecase type
    (:input  (logior pollin pollrdhup pollpri))
    (:output (logior pollout))
    (:io     (logior pollin pollrdhup pollpri pollout))))

(defun process-poll-revents (fd event-type revents)
  (flet ((poll-error ()
           (error 'poll-error :code ebadfd :identifier :ebadfd
                  :event-type event-type :os-handle fd
                  :message "invalid OS handle")))
    (let ((readp nil) (writep nil))
      (flags-case revents
        ((pollin pollrdhup pollpri)
         (setf readp t))
        ((pollout pollhup) (setf writep t))
        ((pollerr) (poll-error))
        ((pollnval) (poll-error)))
      (values readp writep))))

(defun timeout->milisec (timeout)
  (if timeout
      (multiple-value-bind (sec usec)
          (decode-timeout timeout)
        (+ (* sec 1000) (truncate usec 1000)))
      -1))

(defun %poll (fds timeout)
  (isys:repeat-upon-condition-decreasing-timeout
      ((eintr) remaining-time timeout)
    (%sys-poll fds 1 (timeout->milisec remaining-time))))

(defun poll (fd event-type timeout)
  "Poll file descriptor `FD' for I/O readiness. `EVENT-TYPE' must be either :INPUT, :OUTPUT or :IO.
`TIMEOUT' must be either a non-negative integer measured in seconds, or `NIL' meaning no timeout at all.
If a timeout occurs `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readability and writeability of `FD'."
  (flet ((poll-error (posix-err)
           (error 'poll-error
                  :code (code-of posix-err) :identifier (identifier-of posix-err)
                  :event-type event-type :os-handle fd
                  :message (format nil "OS error ~A" (identifier-of posix-err)))))
    (with-foreign-object (pollfd 'pollfd)
      (%sys-bzero pollfd size-of-pollfd)
      (with-foreign-slots ((fd events revents) pollfd pollfd)
        (setf fd fd
              events (compute-poll-flags event-type))
        (handler-case
            (cond
              ((plusp (%poll pollfd timeout))
               (process-poll-revents fd event-type revents))
              (t
               (error 'poll-timeout :os-handle fd :event-type event-type)))
          (posix-error (err) (poll-error err)))))))

(defun poll-file (file-descriptor event-type &optional timeout)
  (poll file-descriptor event-type timeout))
