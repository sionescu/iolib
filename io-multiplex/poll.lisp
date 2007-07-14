;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :io.multiplex)

(defconstant +poll-priority+ 2)

(define-multiplexer poll-multiplexer +poll-priority+ (multiplexer)
  ((fd-set :initform (allocate-pollfd-set) :accessor fd-set-of)
   (fd-set-size :initform 5 :accessor fd-set-size-of)
   (fd-count :initform 0 :accessor fd-count-of)))

(defun allocate-pollfd-set (&optional (count 5))
  (let ((fds (foreign-alloc 'et:pollfd :count count)))
    (et:bzero fds (* et:size-of-pollfd count))
    fds))

(defmethod print-object ((mux poll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "poll(2) multiplexer")))

(defmethod close-multiplexer progn ((mux poll-multiplexer))
  (foreign-free (fd-set-of mux))
  (setf (fd-set-of mux) nil))

(defvar *pollfd-table* (make-hash-table :test #'eql))

(defun calc-pollfd-flags (readp writep)
  (let ((flags 0))
    (when readp (setf flags (logior et:pollin et::pollrdhup et:pollpri)))
    (when writep (setf flags (logior flags et:pollout et:pollhup)))
    flags))

(defun set-pollfd-entry (fd-set index fd readp writep)
  (with-foreign-slots ((et:fd et:events et:revents)
                       (mem-aref fd-set 'et:pollfd index)
                       et:pollfd)
    (setf et:fd fd
          et:revents 0
          et:events (calc-pollfd-flags readp writep))))

(defun extend-pollfd-set (fd-set size)
  (let* ((new-size (+ size 5))
         (new-fd-set (foreign-alloc 'et:pollfd :count new-size)))
    (et:memcpy new-fd-set fd-set (* size et:size-of-pollfd))
    (foreign-free fd-set)
    (values new-fd-set new-size)))

(defmethod monitor-fd ((mux poll-multiplexer) fd-entry)
  (let ((fd (fd-entry-fd fd-entry))
        (readp (not (queue-empty-p (fd-entry-read-events fd-entry))))
        (writep (not (queue-empty-p (fd-entry-write-events fd-entry)))))
    (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                     (count fd-count-of)) mux
      (when (= count size)
        (setf (values fd-set size) (extend-pollfd-set fd-set size)))      
      (set-pollfd-entry fd-set count fd readp writep)
      (setf (gethash fd *pollfd-table*) count)
      (incf count))))

(defmethod update-fd ((mux poll-multiplexer) fd-entry)
  (let* ((fd (fd-entry-fd fd-entry))
         (pos (gethash fd *pollfd-table*))
         (readp (not (queue-empty-p (fd-entry-read-events fd-entry))))
         (writep (not (queue-empty-p (fd-entry-write-events fd-entry)))))
    (with-accessors ((fd-set fd-set-of)) mux
      (set-pollfd-entry fd-set pos fd readp writep))))

(defun shrink-pollfd-set (fd-set count size pos)
  (let* ((new-size (if (> 5 (- size count)) (- size 5) size))
         (new-fd-set (foreign-alloc 'et:pollfd :count new-size)))
    (when (plusp pos)
      (et:memcpy new-fd-set fd-set (* pos et:size-of-pollfd)))
    (when (< pos count)
      (et:memcpy new-fd-set fd-set (* (- count pos) et:size-of-pollfd)))
    (foreign-free fd-set)
    (values new-fd-set new-size)))

(defmethod unmonitor-fd ((mux poll-multiplexer) fd-entry)
  (let* ((fd (fd-entry-fd fd-entry))
         (pos (gethash fd *pollfd-table*)))
    (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                     (count fd-count-of)) mux
      (setf (values fd-set size) (shrink-pollfd-set fd-set (1- count) size pos))
      (remhash fd *pollfd-table*)
      (decf count))))

(defmethod harvest-events ((mux poll-multiplexer) timeout)
  (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                   (count fd-count-of)) mux
    ;; if there are no fds set and timeout is NULL
    ;; poll() blocks forever
    (when (and (zerop count)
               (null timeout))
      (warn "Non fds to monitor and no timeout set !")
      (return-from harvest-events nil))
    ;; FIXME: when does poll() return EBADF ?
    (et:repeat-upon-condition-decreasing-timeout
        ((et:eintr) tmp-timeout timeout)
      (et:poll fd-set count (timeout->milisec tmp-timeout)))
    (harvest-pollfd-events fd-set count)))

(defun harvest-pollfd-events (fd-set count)
  (macrolet ((pollfd-slot (name index)
               `(foreign-slot-value (mem-aref fd-set 'et:pollfd ,index)
                                    'et:pollfd ,name)))
    (loop :for i :below count
          :for event := ()
          :for fd := (pollfd-slot 'et:fd i)
          :for revents := (pollfd-slot 'et:revents i)
       :do (flags-case revents
             ((et:pollout et:pollhup)              (push :write event))
             ((et:pollin et::pollrdhup et:pollpri) (push :read event))
             ((et:pollerr et:pollnval)             (push :error event)))
       :when event :collect (list fd event))))
