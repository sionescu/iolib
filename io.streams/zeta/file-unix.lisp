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
;;; File PRINT-OBJECT
;;;-----------------------------------------------------------------------------

(defmethod print-object ((file file-device) stream)
  (print-unreadable-object (file stream :identity t :type nil)
    (format stream "File device for ~S" (filename-of file))))


;;;-----------------------------------------------------------------------------
;;; File Constructors
;;;-----------------------------------------------------------------------------

(defmethod initialize-instance :after ((device file-device)
                                       &key filename (direction :input)
                                       (if-exists :default) (if-does-not-exist :default)
                                       truncate append (extra-flags 0)
                                       (mode #o666))
  (when (and (eql :error if-exists)
             (eql :error if-does-not-exist))
    (error 'program-error))
  (let ((flags 0))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-direction direction flags if-exists if-does-not-exist))
    (setf (values flags if-exists if-does-not-exist)
          (process-file-flags direction flags if-exists if-does-not-exist
                              truncate append extra-flags))
    (setf (filename-of device) (copy-seq filename)
          (direction-of device) direction
          (if-exists-of device) if-exists
          (if-does-not-exist-of device) if-does-not-exist)
    (with-device (device)
      (device-open device :filename filename :flags flags
                   :mode mode :if-exists if-exists
                   :if-does-not-exist if-does-not-exist))))


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
                 (%sys-unlink filename)
               (posix-error (c) (handle-error c))))
           (try-open (&optional (retry-on-unlink t))
             (handler-case
                 (%sys-open filename flags mode)
               (eexist (c)
                 (cond ((and retry-on-unlink (eql :unlink if-exists))
                        (try-unlink) (try-open nil))
                       (t (handle-error c))))
               (posix-error (c)
                 (handle-error c))
               (:no-error (fd) fd))))
    (let ((fd (try-open)))
      (%set-fd-nonblock fd)
      (setf (handle-of device) fd)))
  (values device))

(defun process-file-direction (direction flags if-exists if-does-not-exist)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (when (eql :default if-exists) (setf if-exists :overwrite))
    (ecase direction
      (:input
       (add-flags o-rdonly)
       (check-type if-exists (member :overwrite :error-if-symlink))
       (check-type if-does-not-exist (member :default :error))
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :error)))
      ((:output :io)
       (add-flags (if (eql :io direction) o-rdwr o-wronly))
       (check-type if-exists file-if-exists)
       (check-type if-does-not-exist file-if-does-not-exist)
       (when (eql :default if-does-not-exist) (setf if-does-not-exist :create))))
    (values flags if-exists if-does-not-exist)))

(defun process-file-flags (direction flags if-exists if-does-not-exist
                           truncate append extra-flags)
  (macrolet ((add-flags (&rest %flags)
               `(setf flags (logior flags ,@%flags))))
    (case if-exists
      (:error
       (unless (eql :input direction) (add-flags o-excl)))
      (:error-if-symlink
       (add-flags o-nofollow)))
    (case if-does-not-exist
      (:create (add-flags o-creat)))
    (cond
      (truncate
       (unless (eql :input direction) (add-flags o-trunc)))
      (append
       (when (eql :output direction) (add-flags o-append)))
      (extra-flags
       (add-flags extra-flags))))
  (values flags if-exists if-does-not-exist))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-CLOSE
;;;-----------------------------------------------------------------------------

(defmethod relinquish ((device file-device) &key abort)
  (declare (ignore abort))
  (%sys-close (handle-of device))
  (setf (handle-of device) nil)
  (values device))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-POSITION
;;;-----------------------------------------------------------------------------

(defmethod device-position ((device file-device))
  (handler-case
      (%sys-lseek (handle-of device) 0 seek-cur)
    (posix-error (err)
      (posix-file-error err device "seeking on"))))

(defmethod (setf device-position) (position (device file-device) &optional (from :start))
  (handler-case
      (%sys-lseek (handle-of device) position
                  (ecase from
                    (:start seek-set)
                    (:current seek-cur)
                    (:end seek-end)))
    (posix-error (err)
      (posix-file-error err device "seeking on"))))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-LENGTH
;;;-----------------------------------------------------------------------------

(defmethod device-length ((device file-device))
  (handler-case
      (%sys-fstat (handle-of device))
    (posix-error (err)
      (posix-file-error err device "getting status of"))))


;;;-----------------------------------------------------------------------------
;;; I/O WAIT
;;;-----------------------------------------------------------------------------

(defmethod device-poll-input ((device file-device) &optional timeout)
  (poll-fd (handle-of device) :input timeout))

(defmethod device-poll-output ((device file-device) &optional timeout)
  (poll-fd (handle-of device) :output timeout))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read/non-blocking ((device file-device) vector start end)
  (with-device (device)
    (%read-octets/non-blocking (handle-of device) vector start end)))

(defmethod device-read/timeout ((device file-device) vector start end timeout)
  (with-device (device)
    (%read-octets/timeout (handle-of device) vector start end timeout)))


;;;-----------------------------------------------------------------------------
;;; File DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write/non-blocking ((device file-device) vector start end)
  (with-device (device)
    (%write-octets/non-blocking (handle-of device) vector start end)))

(defmethod device-write/timeout ((device file-device) vector start end timeout)
  (with-device (device)
    (%write-octets/timeout (handle-of device) vector start end timeout)))


;;;-----------------------------------------------------------------------------
;;; OPEN-FILE
;;;-----------------------------------------------------------------------------

(defun open-file (filename &key (direction :input)
                  (if-exists :default) (if-does-not-exist :default)
                  truncate append (extra-flags 0) (mode #o666))
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
                     :extra-flags extra-flags
                     :mode mode)
    (posix-file-error (error)
      (case (identifier-of error)
        (:enoent
         (if (null if-does-not-exist) nil (error error)))
        (:eexist
         (if (null if-exists) nil (error error)))
        (t (error error))))
    (:no-error (file) file)))
