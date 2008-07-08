;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- File devices.
;;;

(in-package :io.zeta-streams)

(deftype file-direction ()
  '(member :input :output :io))

(deftype file-if-exists ()
  '(member :default :error :error-if-symlink :unlink :overwrite))

(deftype file-if-does-not-exist ()
  '(member :default :error :create))

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
    (device-open device :filename filename :flags flags
                 :mode mode :if-exists if-exists
                 :if-does-not-exist if-does-not-exist)))

(defmethod device-open ((device file-device) &key filename flags mode
                        if-exists if-does-not-exist &allow-other-keys)
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
            (output-handle-of device) fd)
      (when (logtest flags nix:o-append)
        (setf (position-of device) (device-length device)))))
  (values device))

(defun process-file-direction (direction flags if-exists if-does-not-exist)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (ecase direction
      (:input
       (add-flags nix:o-rdonly)
       (check-type if-exists (member nil :error-if-symlink))
       (check-type if-does-not-exist (member nil :error))
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :error)))
      (:output
       (add-flags nix:o-wronly)
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (when (eql :default if-exists) (setf if-exists :overwrite))
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :create)))
      (:io
       (add-flags nix:o-rdwr)
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (when (eql :default if-exists) (setf if-exists :overwrite))
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
       (add-flags nix:o-nofollow)
       (setf if-exists :overwrite)))
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

(defmethod device-close ((device file-device))
  (ignore-errors (nix:close (input-handle-of device)))
  (setf (input-handle-of device) nil
        (output-handle-of device) nil)
  (values device))

(defmethod device-position ((device file-device))
  (position-of device))

(defmethod (setf device-position) (position (device file-device) &key (from :start))
  (ecase from
    (:start
     (nix:lseek (input-handle-of device) position nix:seek-set)
     (setf (position-of device) position))
    (:current
     (nix:lseek (input-handle-of device) position nix:seek-cur)
     (incf (position-of device) position))
    (:end
     (nix:lseek (input-handle-of device) position nix:seek-end)
     ;; WARNING: possible race condition here: if betweek lseek() and fstat()
     ;; the file size is modified, this calculation will be wrong
     ;; perhaps fcntl() or ioctl() can be used to find out the file offset
     (setf (position-of device) (+ (device-length device) position))))
  (position-of device))

(defmethod device-length ((device file-device))
  (nix:stat-size (nix:fstat (input-handle-of device))))

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
