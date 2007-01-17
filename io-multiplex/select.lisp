;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006,2007 by Stelian Ionescu                            ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :io.multiplex)

(defconstant +select-priority+ 3)

(define-multiplexer select-multiplexer +select-priority+
  (multiplexer) ())

(defun select-setup-masks (select-iface read-fds write-fds except-fds)
  (declare (type et:foreign-pointer
                 read-fds write-fds except-fds))

  (et:fd-zero read-fds)
  (et:fd-zero write-fds)
  (et:fd-zero except-fds)

  (let ((max-fd 0))
    (with-hash-table-iterator (next-item (fd-entries select-iface))
      (multiple-value-bind (item-p fd fd-entry) (next-item)
        (when item-p
          (when (> fd max-fd)
            (setf max-fd fd))
          (when (fd-entry-read-handlers fd-entry)
            (et:fd-set fd read-fds))
          (when (fd-entry-write-handlers fd-entry)
            (et:fd-set fd write-fds))
          (when (fd-entry-except-handlers fd-entry)
            (et:fd-set fd except-fds)))))
    max-fd))

(defun handle-select-fd-errors (select-iface)
  (let ((current-entries (fd-entries select-iface))
        invalid-fd-entries)
    (with-hash-table-iterator (next-item current-entries)
      (multiple-value-bind (item-p fd fd-entry) (next-item)
        (when (and item-p (not (fd-open-p fd)))
          (push fd-entry invalid-fd-entries))))
    (dolist (fd-entry invalid-fd-entries)
      (let ((fd (fd-entry-fd fd-entry))
            (error-handlers (fd-entry-error-handlers fd-entry)))
        (if error-handlers
            (dolist (error-handler error-handlers)
              (funcall (handler-function error-handler) fd :error))
            (remhash fd current-entries))))))

(defmethod serve-fd-events ((mux select-multiplexer)
                            &key timeout)
  (with-foreign-objects ((read-fds 'et:fd-set)
                         (write-fds 'et:fd-set)
                         (except-fds 'et:fd-set))

    (let ((max-fd (select-setup-masks
                   mux
                   read-fds
                   write-fds
                   except-fds))
          (count 0))

      ;; this means there are no valid fds to serve
      ;; but with no fds active select() blocks forever(at least on Linux)
      (when (zerop max-fd)
        (return-from serve-fd-events 0))

      (with-accessors ((fd-entries fd-entries)) mux
        (tagbody
         :start
           (handler-case
               (with-foreign-object (to 'et:timeval)
                 (when timeout
                   (progn
                     (et:memset to 0 #.(foreign-type-size 'et:timeval))
                     (multiple-value-bind
                           (to-sec to-usec) (decode-timeout timeout)
                       (setf (foreign-slot-value to 'et:timeval 'et:tv-sec) to-sec)
                       (setf (foreign-slot-value to 'et:timeval 'et:tv-usec) to-usec))))
                 (et:select (1+ max-fd)
                            read-fds
                            write-fds
                            except-fds
                            (if timeout to (null-pointer))))
             (et:unix-error-intr (err)
               (declare (ignore err))
               (go :start))
             (et:unix-error-badf (err)
               (declare (ignore err))
               (handle-select-fd-errors mux))))

        (with-hash-table-iterator (next-item fd-entries)
          (multiple-value-bind (item-p fd fd-entry) (next-item)
            (when item-p
              (if (fd-open-p fd)
                  (progn
                    (incf count)
                    (when (and (et:fd-isset fd except-fds)
                               (fd-entry-except-handlers fd-entry))
                      (dolist (except-handler (fd-entry-except-handlers fd-entry))
                        (funcall (handler-function except-handler) fd :except)))
                    (when (and (et:fd-isset fd read-fds)
                               (fd-entry-read-handlers fd-entry))
                      (dolist (read-handler (fd-entry-read-handlers fd-entry))
                        (funcall (handler-function read-handler) fd :read)))
                    (when (and (et:fd-isset fd write-fds)
                               (fd-entry-write-handlers fd-entry))
                      (dolist (write-handler (fd-entry-write-handlers fd-entry))
                        (funcall (handler-function write-handler) fd :write))))
                  ;; TODO: add better error handling
                  (error "Handler for bad fd is present: ~A " fd)))))
        (return-from serve-fd-events count)))))
