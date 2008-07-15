;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- File devices.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; File Classes and Types
;;;-----------------------------------------------------------------------------

(defclass file-device (single-channel-device)
  ((filename :initarg :filename :accessor filename-of)
   (direction :initarg :direction :accessor direction-of)
   (if-exists :initarg :if-exists :accessor if-exists-of)
   (if-does-not-exist :initarg :if-does-not-exist :accessor if-does-not-exist-of)))

(defclass memory-mapped-file-device (file-device direct-device) ())

(deftype file-direction ()
  '(member :input :output :io))

(deftype file-if-exists ()
  '(member :default :error :error-if-symlink :unlink :overwrite))

(deftype file-if-does-not-exist ()
  '(member :default :error :create))


;;;-----------------------------------------------------------------------------
;;; File Constructors
;;;-----------------------------------------------------------------------------

(defmethod initialize-instance :after ((device file-device)
                                       &key filename (direction :input)
                                       (if-exists :default) (if-does-not-exist :default)
                                       truncate append nonblocking (extra-flags 0)
                                       (mode #o666))
  (when (and (eql :error if-exists)
             (eql :error if-does-not-exist))
    (error 'program-error))
  (let ((flags 0))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-direction direction flags if-exists if-does-not-exist))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-flags direction flags if-exists if-does-not-exist
                              truncate append nonblocking extra-flags))
    (setf (filename-of device) (copy-seq filename)
          (direction-of device) direction
          (if-exists-of device) if-exists
          (if-does-not-exist-of device) if-does-not-exist)
    (with-device (device)
      (device-open device :filename filename :flags flags
                   :mode mode :if-exists if-exists
                   :if-does-not-exist if-does-not-exist))))


;;;-----------------------------------------------------------------------------
;;; File PRINT-OBJECT
;;;-----------------------------------------------------------------------------

(defmethod print-object ((file file-device) stream)
  (print-unreadable-object (file stream :identity t :type nil)
    (format stream "File device for ~S" (filename-of file))))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-OPEN
;;;-----------------------------------------------------------------------------

(defmethod device-open ((device file-device) &key filename flags mode
                        if-exists if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (labels ((handle-error (c)
             (posix-file-error c filename "opening"))
           (try-unlink ()
             (handler-case
                 (nix:unlink filename)
               (nix:posix-error (c) (handle-error c))))
           (try-open (&optional (retry-on-unlink t))
             (handler-case
                 (nix:open filename flags mode)
               (nix:eexist (c)
                 (cond ((and retry-on-unlink (eql :unlink if-exists))
                        (try-unlink) (try-open nil))
                       (t (handle-error c))))
               (nix:posix-error (c)
                 (handle-error c))
               (:no-error (fd) fd))))
    (let ((fd (try-open)))
      (%set-fd-nonblock-mode fd t)
      (setf (input-handle-of device) fd
            (output-handle-of device) fd)))
  (values device))

(defun process-file-direction (direction flags if-exists if-does-not-exist)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (when (eql :default if-exists) (setf if-exists :overwrite))
    (ecase direction
      (:input
       (add-flags nix:o-rdonly)
       (check-type if-exists (member :overwrite :error-if-symlink))
       (check-type if-does-not-exist (member :default :error))
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :error)))
      ((:output :io)
       (add-flags (if (eql :io direction) nix:o-rdwr nix:o-wronly))
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :create))))
    (values flags if-exists if-does-not-exist)))

(defun process-file-flags (direction flags if-exists if-does-not-exist
                           truncate append nonblocking extra-flags)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (case if-exists
      (:error
       (unless (eql :input direction) (add-flags nix:o-excl)))
      (:error-if-symlink
       (add-flags nix:o-nofollow)))
    (case if-does-not-exist
      (:create (add-flags nix:o-creat)))
    (cond
      (truncate
       (unless (eql :input direction) (add-flags nix:o-trunc)))
      (append
       (when (eql :output direction) (add-flags nix:o-append)))
      (nonblocking
       (add-flags nix:o-nonblock))
      (extra-flags
       (add-flags extra-flags))))
  (values flags if-exists if-does-not-exist))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-CLOSE
;;;-----------------------------------------------------------------------------

(defmethod device-close ((device file-device) &optional abort)
  (declare (ignore abort))
  (ignore-errors (nix:close (input-handle-of device)))
  (setf (input-handle-of device) nil
        (output-handle-of device) nil)
  (values device))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-POSITION
;;;-----------------------------------------------------------------------------

(defmethod device-position ((device file-device))
  (handler-case
      (nix:lseek (input-handle-of device) 0 nix:seek-cur)
    (nix:posix-error (err)
      (posix-file-error err device "seeking on"))))

(defmethod (setf device-position) (position (device file-device) &key (from :start))
  (handler-case
      (nix:lseek (input-handle-of device) position
                 (ecase from
                   (:start nix:seek-set)
                   (:current nix:seek-cur)
                   (:end nix:seek-end)))
    (nix:posix-error (err)
      (posix-file-error err device "seeking on"))))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-LENGTH
;;;-----------------------------------------------------------------------------

(defmethod device-length ((device file-device))
  (handler-case
      (nix:stat-size (nix:fstat (input-handle-of device)))
    (nix:posix-error (err)
      (posix-file-error err device "getting status of"))))


;;;-----------------------------------------------------------------------------
;;; OPEN-FILE
;;;-----------------------------------------------------------------------------

(defun open-file (filename &key (direction :input)
                  (if-exists :default) (if-does-not-exist :default)
                  truncate append nonblocking (extra-flags 0) (mode #o666))
  (when (and (null if-exists)
             (null if-does-not-exist))
    (error 'program-error))
  (handler-case
      (make-instance 'file-device
                     :filename (namestring filename)
                     :direction direction
                     :if-exists if-exists
                     :if-does-not-exist if-does-not-exist
                     :truncate truncate
                     :append append
                     :nonblocking nonblocking
                     :extra-flags extra-flags
                     :mode mode)
    (posix-file-error (error)
      (case (posix-file-error-identifier error)
        (:enoent
         (if (null if-does-not-exist) nil (error error)))
        (:eexist
         (if (null if-exists) nil (error error)))
        (t (error error))))
    (:no-error (file) file)))
