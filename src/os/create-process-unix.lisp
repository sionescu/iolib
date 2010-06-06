;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wrapper over posix_spawn(3)
;;;

(in-package :iolib.os)

(defclass process ()
  ((pid    :initarg :pid :reader process-pid)
   (stdin  :initform nil :reader process-stdin)
   (stdout :initform nil :reader process-stdout)
   (stderr :initform nil :reader process-stderr)))

(defmethod initialize-instance :after ((process process) &key stdin stdout stderr)
  (with-slots ((in stdin) (out stdout) (err stderr))
      process
    (setf in (make-instance 'iolib.streams:dual-channel-gray-stream
                            :output-fd stdin)
          out (make-instance 'iolib.streams:dual-channel-gray-stream
                             :input-fd stdout)
          err (make-instance 'iolib.streams:dual-channel-gray-stream
                             :input-fd stderr))))

(defmethod close ((process process) &key abort)
  (with-slots (pid stdin stdout stderr)
      process
    (close stdin :abort abort)
    (close stdout :abort abort)
    (close stderr :abort abort)
    (setf pid nil stdin nil stdout nil stderr nil)))

(defmethod print-object ((o process) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s ":pid ~S" (process-pid o))))

(defmacro with-posix-spawn-arguments ((attributes file-actions pid) &body body)
  (with-gensyms (spawnattr-initialized-p file-actions-initialized-p)
    `(with-foreign-objects ((,attributes 'posix-spawnattr-t)
                            (,file-actions 'posix-spawn-file-actions-t)
                            (,pid 'pid-t))
       (let (,spawnattr-initialized-p
             ,file-actions-initialized-p)
         (unwind-protect
              (progn
                (posix-spawnattr-init ,attributes)
                (setf ,spawnattr-initialized-p t)
                (posix-spawn-file-actions-init ,file-actions)
                (setf ,file-actions-initialized-p t)
                ,@body)
           (when ,spawnattr-initialized-p
             (posix-spawnattr-destroy ,attributes))
           (when ,file-actions-initialized-p
             (posix-spawn-file-actions-destroy ,file-actions)))))))

(defun allocate-argv (argv argc program arglist)
  ;; copy program name
  (setf (mem-aref argv :pointer 0)
        (foreign-string-alloc program))
  ;; copy program arguments
  (loop :for i :from 1
        :for arg :in arglist :do
        (setf (mem-aref argv :pointer i)
              (foreign-string-alloc arg)))
  ;; final null pointer
  (setf (mem-aref argv :pointer (1- argc)) (null-pointer)))

(defmacro with-argv ((argv program arguments) &body body)
  (with-gensyms (argc)
    `(let ((,argc (+ 2 (length ,arguments))))
       (with-foreign-object (,argv :pointer ,argc)
         (unwind-protect
              (progn
                (allocate-argv ,argv ,argc ,program ,arguments)
                ,@body)
           (deallocate-null-ended-list ,argv))))))

(defmacro with-3-pipes ((stdin-r stdin-w stdout-r stdout-w stderr-r stderr-w)
                        &body body)
  `(multiple-value-bind (,stdin-r ,stdin-w) (isys:pipe)
     (multiple-value-bind (,stdout-r ,stdout-w) (isys:pipe)
       (multiple-value-bind (,stderr-r ,stderr-w) (isys:pipe)
         (unwind-protect-case ()
             (progn ,@body)
           ;; These are the FDs that we would use on the Lisp side
           (:abort
            (isys:close ,stdin-w)
            (isys:close ,stdout-r)
            (isys:close ,stderr-r))
           ;; These FDs are shared with the subprocess, must be closed always
           (:always
            (isys:close ,stdin-r)
            (isys:close ,stdout-w)
            (isys:close ,stderr-w)))))))

(defun create-process (program &optional arguments &key (search t) environment
                               ;; path uid gid effective
                               )
  (with-posix-spawn-arguments (attributes file-actions pid)
    (with-argv (argv program arguments)
      (with-c-environment (environment)
        (with-3-pipes (stdin-r stdin-w stdout-r stdout-w stderr-r stderr-w)
          (posix-spawn-file-actions-adddup2 file-actions stdin-r +stdin+)
          (posix-spawn-file-actions-adddup2 file-actions stdout-w +stdout+)
          (posix-spawn-file-actions-adddup2 file-actions stderr-w +stderr+)
          (with-foreign-string (cfile program)
            (if search
                (posix-spawnp pid cfile file-actions attributes argv isys:*environ*)
                (posix-spawn  pid cfile file-actions attributes argv isys:*environ*)))
          (make-instance 'process :pid (mem-ref pid 'pid-t)
                         :stdin stdin-w :stdout stdout-r :stderr stderr-r))))))

(defun run-program (program &optional arguments &key (search t) environment)
  (flet ((slurp-stream-into-string (stream)
           (with-output-to-string (s)
             (loop :for c := (read-char stream nil nil)
                   :while c :do (write-char c s)))))
    (let ((process (create-process program arguments
                                   :search search
                                   :environment environment)))
      (values (process-wait process)
              (slurp-stream-into-string (process-stdout process))
              (slurp-stream-into-string (process-stderr process))))))

(defun process-wait (process)
  (isys:waitpid (process-pid process) 0))

(defun process-kill (process signum)
  (isys:kill (process-pid process) signum))
