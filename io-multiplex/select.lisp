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

(defconstant +select-priority+ 3)


(define-multiplexer select-multiplexer +select-priority+ (multiplexer)
  ((max-fd :initform 0
           :accessor max-fd-of)
   (read-fd-set :initform (allocate-fd-set)
                :reader read-fd-set-of)
   (write-fd-set :initform (allocate-fd-set)
                 :reader write-fd-set-of)
   (except-fd-set :initform (allocate-fd-set)
                  :reader except-fd-set-of))
  (:default-initargs :fd-limit (1- et:fd-setsize)))


(defun allocate-fd-set ()
  (et:fd-zero (foreign-alloc 'et:fd-set)))


(defmethod print-object ((mux select-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "select(2) multiplexer")))


(defmethod close-multiplexer progn ((mux select-multiplexer))
  (foreign-free (read-fd-set-of mux))
  (foreign-free (write-fd-set-of mux))
  (foreign-free (except-fd-set-of mux))
  (mapc #'(lambda (slot)
            (slot-makunbound mux slot))
        '(max-fd read-fd-set write-fd-set except-fd-set)))


(defun find-max-fd (fd-set end)
  (loop :for i :downfrom end :to 0
     :do (when (et:fd-isset i fd-set)
           (return-from find-max-fd i)))
  ;; this means no fd <= end is set
  -1)


(defun recalc-fd-masks (mux fd read write)
  (with-accessors ((rs read-fd-set-of) (ws write-fd-set-of)
                   (es except-fd-set-of) (max-fd max-fd-of)) mux
    (if read
        (progn
          (et:fd-set fd rs)
          (et:fd-set fd es))
        (progn
          (et:fd-clr fd rs)
          (et:fd-clr fd es)))
    (if write
        (et:fd-set fd ws)
        (et:fd-clr fd ws))
    (setf max-fd (max (find-max-fd rs fd)
                      (find-max-fd ws fd)))
    t))


(defmethod monitor-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (not (queue-empty-p (fd-entry-read-events fd-entry)))
                   (not (queue-empty-p (fd-entry-write-events fd-entry)))))


(defmethod update-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (not (queue-empty-p (fd-entry-read-events fd-entry)))
                   (not (queue-empty-p (fd-entry-write-events fd-entry)))))


(defmethod unmonitor-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry) nil nil))


(defmethod harvest-events ((mux select-multiplexer) timeout)
  (with-accessors ((rs read-fd-set-of) (ws write-fd-set-of)
                   (es except-fd-set-of) (max-fd max-fd-of)) mux
    ;; if there are no fds set and timeout is NULL
    ;; select() blocks forever
    (when (and (minusp max-fd)
               (null timeout))
      (warn "Non fds to monitor and no timeout set !")
      (return-from harvest-events nil))

    (with-foreign-objects ((read-fds 'et:fd-set)
                           (write-fds 'et:fd-set)
                           (except-fds 'et:fd-set))
      (et:copy-fd-set rs read-fds)
      (et:copy-fd-set ws write-fds)
      (et:copy-fd-set es except-fds)

      (handler-case
          (with-foreign-object (tv 'et:timeval)
            (when timeout
              (timeout->timeval timeout tv))
            (et:select (1+ max-fd)
                       read-fds
                       write-fds
                       except-fds
                       (if timeout tv (null-pointer))))
        (et:unix-error-badf (err)
          (declare (ignore err))
          (return-from harvest-events
            (harvest-select-fd-errors rs ws max-fd))))

      (harvest-select-events max-fd read-fds write-fds except-fds))))


(defun harvest-select-events (max-fd read-fds write-fds except-fds)
  (loop :for fd :upto max-fd
     :for event := () :then ()
     :when (or (et:fd-isset fd read-fds)
               (et:fd-isset fd except-fds)) :do (push :read event)
     :when (et:fd-isset fd write-fds) :do (push :write event)
     :when event :collect (list fd event)))


;; FIXME: I don't know whether on all *nix systems select()
;; returns EBADF only when a given FD present in some fd-set
;; is closed(as the POSIX docs say) or if some other kinds of
;; errors are reported too(as the Linux manpages seem to suggest)
(defun fd-error-p (fd)
  (not (et:fd-open-p fd)))

(defun harvest-select-fd-errors (read-fds write-fds max-fd)
  (loop :for fd :upto max-fd
     :when (and (or (et:fd-isset fd read-fds)
                    (et:fd-isset fd write-fds))
                (fd-error-p fd))
     :collect (cons fd :error)))
