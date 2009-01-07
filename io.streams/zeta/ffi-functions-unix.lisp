;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *NIX-specific routines.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; FD polling
;;;-------------------------------------------------------------------------

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
    (let ((readp  nil) (rhupp nil)
          (writep nil) (whupp nil))
      (flags-case revents
        ((pollin pollpri)   (setf readp t))
        ((pollrdhup)        (setf rhupp t))
        ((pollout)          (setf writep t))
        ((pollhup)          (setf whupp t))
        ((pollerr pollnval) (poll-error)))
      (values readp rhupp writep whupp))))

(defun timeout->milisec (timeout)
  (multiple-value-bind (sec usec)
      (decode-timeout timeout)
    (+ (* sec 1000) (truncate usec 1000))))

(defun %poll (fds timeout)
  (repeat-upon-condition-decreasing-timeout
      ((eintr) remaining-time timeout)
    (%sys-poll fds 1 (timeout->milisec remaining-time))))

(defun poll-fd (file-descriptor event-type timeout)
  "Poll file descriptor `FD' for I/O readiness. `EVENT-TYPE' must be either
:INPUT, :OUTPUT or :IO. `TIMEOUT' must be a non-negative real measured
in seconds. If a timeout occurs `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readability and writeability of `FD'."
  (flet ((poll-error (posix-err)
           (error 'poll-error
                  :code (code-of posix-err)
                  :identifier (identifier-of posix-err)
                  :event-type event-type
                  :os-handle file-descriptor
                  :message (format nil "OS error ~A"
                                   (identifier-of posix-err)))))
    (with-foreign-object (pollfd 'pollfd)
      (%sys-bzero pollfd size-of-pollfd)
      (with-foreign-slots ((fd events revents) pollfd pollfd)
        (setf fd file-descriptor
              events (compute-poll-flags event-type))
        (handler-case
            (cond
              ((plusp (%poll pollfd timeout))
               (process-poll-revents fd event-type revents))
              (t
               (error 'poll-timeout
                      :os-handle file-descriptor
                      :event-type event-type)))
          (posix-error (err) (poll-error err)))))))


;;;-------------------------------------------------------------------------
;;; Set FD nonblocking
;;;-------------------------------------------------------------------------

(defun %set-fd-nonblock (fd)
  (declare (special *device*))
  (handler-case
      (with-foreign-object (arg :int)
        (setf (mem-aref arg :int) 1)
        (%sys-ioctl/3 fd fionbio arg))
    (posix-error (err)
      (posix-file-error err *device* "issuing FIONBIO IOCTL on")))
  (values))


;;;-------------------------------------------------------------------------
;;; Get number of bytes availabe on FD
;;;-------------------------------------------------------------------------

(defun %get-fd-nbytes (fd)
  (declare (special *device*))
  (handler-case
      (with-foreign-object (arg :int)
        (%sys-ioctl/3 fd fionread arg)
        (mem-aref arg :int))
    (posix-error (err)
      (posix-file-error err *device* "issuing FIONREAD IOCTL on"))))


;;;-------------------------------------------------------------------------
;;; File Descriptor reading
;;;-------------------------------------------------------------------------

(defun %read-octets/non-blocking (fd vector start end)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (%sys-read fd (inc-pointer buf start) (- end start))
      (ewouldblock () 0)
      (posix-error (err)
        (posix-file-error err *device* "reading data from"))
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun %read-octets/timeout (fd vector start end timeout)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (repeat-decreasing-timeout (remaining (clamp-timeout timeout) :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (poll-fd fd :input remaining)
                   (return-from :rloop 0))))
        (handler-case
            (%sys-read fd (inc-pointer buf start) (- end start))
          (ewouldblock () (check-timeout))
          (posix-error (err)
            (posix-file-error err *device* "reading data from"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :eof nbytes))))))))


;;;-------------------------------------------------------------------------
;;; File Descriptor writing
;;;-------------------------------------------------------------------------

(defun %write-octets/non-blocking (fd vector start end)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (%sys-write fd (inc-pointer buf start) (- end start))
      (ewouldblock () 0)
      (epipe () :hangup)
      (posix-error (err)
        (posix-file-error err *device* "writing data to"))
      (:no-error (nbytes)
        (if (zerop nbytes) :hangup nbytes)))))

(defun %write-octets/timeout (fd vector start end timeout)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (repeat-decreasing-timeout (remaining (clamp-timeout timeout) :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (poll-fd fd :output remaining)
                   (return-from :rloop 0))))
        (handler-case
            (%sys-write fd (inc-pointer buf start) (- end start))
          (ewouldblock () (check-timeout))
          (epipe () (return-from :rloop :hangup))
          (posix-error (err)
            (posix-file-error err *device* "writing data to"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :hangup nbytes))))))))
