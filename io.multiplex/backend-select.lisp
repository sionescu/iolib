;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- select(2) multiplexer implementation.
;;;

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
  (:default-initargs :fd-limit (1- fd-setsize)))

(defun allocate-fd-set ()
  (fd-zero (foreign-alloc 'fd-set)))

(defmethod print-object ((mux select-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "select(2) multiplexer")))

(defmethod close-multiplexer progn ((mux select-multiplexer))
  (foreign-free (read-fd-set-of mux))
  (foreign-free (write-fd-set-of mux))
  (foreign-free (except-fd-set-of mux))
  (dolist (slot '(max-fd read-fd-set write-fd-set except-fd-set))
    (setf (slot-value mux slot) nil)))

(defun find-max-fd (fd-set end)
  (loop :for i :downfrom end :to 0
     :do (when (fd-isset i fd-set) (return* i)))
  ;; this means no fd <= end is set
  -1)

(defun recalc-fd-masks (mux fd read write)
  (with-accessors ((rs read-fd-set-of) (ws write-fd-set-of)
                   (es except-fd-set-of) (max-fd max-fd-of)) mux
    (cond (read
           (fd-set fd rs)
           (fd-set fd es))
          (t
           (fd-clr fd rs)
           (fd-clr fd es)))
    (if write
        (fd-set fd ws)
        (fd-clr fd ws))
    (setf max-fd (max (find-max-fd rs fd)
                      (find-max-fd ws fd)))
    t))

(defmethod monitor-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (fd-entry-read-handler fd-entry)
                   (fd-entry-write-handler fd-entry)))

(defmethod update-fd ((mux select-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (fd-entry-read-handler fd-entry)
                   (fd-entry-write-handler fd-entry)))

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
      (return* nil))
    (with-foreign-objects ((read-fds 'fd-set)
                           (write-fds 'fd-set)
                           (except-fds 'fd-set))
      (copy-fd-set rs read-fds)
      (copy-fd-set ws write-fds)
      (copy-fd-set es except-fds)
      (handler-case
          (with-foreign-object (tv 'timeval)
            (nix:repeat-upon-condition-decreasing-timeout
                ((nix:eintr) tmp-timeout timeout)
              (when tmp-timeout
                (timeout->timeval tmp-timeout tv))
              (select (1+ max-fd)
                      read-fds
                      write-fds
                      except-fds
                      (if tmp-timeout tv (null-pointer)))))
        (nix:ebadf ()
          (return* (harvest-select-fd-errors rs ws max-fd))))
      (harvest-select-events max-fd read-fds write-fds except-fds))))

(defun harvest-select-events (max-fd read-fds write-fds except-fds)
  (loop :for fd :upto max-fd
        :for event := () :then ()
        :when (or (fd-isset fd read-fds)
                  (fd-isset fd except-fds)) :do (push :read event)
        :when (fd-isset fd write-fds) :do (push :write event)
        :when event :collect (list fd event)))

;;; FIXME: I don't know whether on all *nix systems select()
;;; returns EBADF only when a given FD present in some fd-set
;;; is closed(as the POSIX docs say) or if some other kinds of
;;; errors are reported too(as the Linux manpages seem to suggest)
(defun fd-error-p (fd)
  (not (nix:fd-open-p fd)))

(defun harvest-select-fd-errors (read-fds write-fds max-fd)
  (loop :for fd :upto max-fd
        :when (and (or (fd-isset fd read-fds)
                       (fd-isset fd write-fds))
                   (fd-error-p fd))
        :collect (cons fd :error)))
