;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- File devices.
;;;

(in-package :io.zeta-streams)

(deftype file-direction ()
  '(member :input :output :io))

(deftype file-if-exists ()
  '(member nil :error :error-if-symlink :unlink :overwrite))

(deftype file-if-does-not-exist ()
  '(member nil :error :create))

(defun file-extra-options-p (options)
  (subsetp options '(:direct :sync) :test #'eq))

(deftype file-extra-options ()
  '(satisfies file-extra-options-p))

(defparameter *extra-open-flags*
  '((:direct . nix:o-direct)
    (:sync . nix:o-sync)))

(defmethod initialize-instance :after ((device file-device)
                                       &key filename (direction :input)
                                       if-exists if-does-not-exist truncate append
                                       nonblocking extra-options (mode #o666))
  (when (and (eql :error if-exists)
             (eql :error if-does-not-exist))
    (error 'program-error))
  (let ((flags 0))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-direction direction flags if-exists if-does-not-exist))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-flags direction flags if-exists if-does-not-exist
                              truncate append nonblocking extra-options))
    (setf (filename-of device) (copy-seq filename)
          (direction-of device) direction
          (if-exists-of device) if-exists
          (if-does-not-exist-of device) if-does-not-exist)
    (prog1
        (device-open device :filename filename :flags flags
                     :mode mode :if-exists if-exists
                     :if-does-not-exist if-does-not-exist)
      (when append
        (setf (slot-value device 'position)
              (device-length device))))))

(defmethod device-open ((device file-device) &key filename flags mode
                        if-exists if-does-not-exist &allow-other-keys)
  (declare (ignore if-does-not-exist))
  (labels ((handle-error (error)
             (error 'posix-file-error
                    :code (osicat-sys:system-error-code error)
                    :identifier (osicat-sys:system-error-identifier error)
                    :pathname filename :action "opening"))
           (try-open (&optional (retry-on-unlink t))
             (handler-case
                 (nix:open filename flags mode)
               (nix:eexist (error)
                 (cond ((and retry-on-unlink (eql :unlink if-exists))
                        (handler-case
                            (nix:unlink filename)
                          (nix:posix-error (error) (handle-error error)))
                        (try-open nil))
                       (t (handle-error error))))
               (nix:posix-error (error)
                 (handle-error error))
               (:no-error (fd) fd))))
    (let ((fd (try-open)))
      (%set-fd-nonblock-mode fd t)
      (setf (input-handle-of device) fd
            (output-handle-of device) fd)))
  (values device))

(defun process-file-direction (direction flags if-exists if-does-not-exist)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (ecase direction
      (:input
       (add-flags nix:o-rdonly)
       (check-type if-exists (member nil :error-if-symlink))
       (check-type if-does-not-exist (member nil :error))
       (unless if-does-not-exist (setf if-does-not-exist :error)))
      (:output
       (add-flags nix:o-wronly)
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (unless if-exists (setf if-exists :overwrite))
       (unless if-does-not-exist (setf if-does-not-exist :create)))
      (:io
       (add-flags nix:o-rdwr)
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (unless if-exists (setf if-exists :overwrite))
       (unless if-does-not-exist (setf if-does-not-exist :create))))
    (values flags if-exists if-does-not-exist)))

(defun compute-extra-flags (options)
  (let ((flags 0))
    (dolist (option options)
      (if-let (value (cdr (assoc option *extra-open-flags*)))
        (setf flags (logior flags value))
        (error "Invalid option: ~A" option)))
    (values flags)))

(defun process-file-flags (direction flags if-exists if-does-not-exist
                           truncate append nonblocking extra-options)
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
      (extra-options
       (add-flags (compute-extra-flags extra-options)))))
  (values flags if-exists if-does-not-exist))

(defmethod device-close ((device file-device))
  (ignore-errors (nix:close (input-handle-of device)))
  (setf (input-handle-of device) nil
        (output-handle-of device) nil)
  (values device))

(defmethod device-position ((device file-device))
  (position-of device))

(defmethod (setf device-position) (offset (device file-device) &rest args)
  (destructuring-bind (type) args
    (ecase type
      (:start
       (nix:lseek (input-handle-of device) offset nix:seek-set)
       (setf (position-of device) offset))
      (:current
       (nix:lseek (input-handle-of device) offset nix:seek-cur)
       (incf (position-of device) offset))
      (:end
       (nix:lseek (input-handle-of device) offset nix:seek-end)
       ;; WARNING: possible race condition here: if betweek lseek() and fstat()
       ;; the file size is modified, this calculation will be wrong
       ;; perhaps fcntl() or ioctl() can be used to find out the file offset
       (setf (position-of device) (+ (device-length device) offset)))))
  (position-of device))

(defmethod device-length ((device file-device))
  (nix:stat-size (nix:fstat (input-handle-of device))))

(defun open-file (filename &key (direction :input)
                  (if-exists nil if-exists-p)
                  (if-does-not-exist nil if-does-not-exist-p)
                  truncate append nonblocking extra-options (mode #o666))
  (handler-case
      (make-instance 'file-device
                     :filename (namestring filename)
                     :direction direction
                     :if-exists (if (and if-exists-p (null if-exists))
                                    :error if-exists)
                     :if-does-not-exist (if (and if-does-not-exist
                                                 (null if-does-not-exist-p))
                                            :error if-does-not-exist)
                     :truncate truncate
                     :append append
                     :nonblocking nonblocking
                     :extra-options extra-options
                     :mode mode)
    (posix-file-error (error)
      (case (posix-file-error-identifier error)
        (:enoent
         (if (null if-does-not-exist) nil (error error)))
        (:eexist
         (if (null if-exists) nil (error error)))
        (t (error error))))
    (:no-error (file) file)))
