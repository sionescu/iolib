;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wrapper over lfp_spawn(3)
;;;

(in-package :iolib.os)

(defclass process ()
  ((pid    :initarg :pid :reader process-pid)
   (reaped :initform nil)
   (stdin  :initform nil :reader process-stdin)
   (stdout :initform nil :reader process-stdout)
   (stderr :initform nil :reader process-stderr)))

(defmethod initialize-instance :after ((process process) &key stdin stdout stderr)
  (with-slots ((in stdin) (out stdout) (err stderr))
      process
    (setf in  (and stdin  (make-instance 'iolib.streams:dual-channel-gray-stream :fd stdin))
          out (and stdout (make-instance 'iolib.streams:dual-channel-gray-stream :fd stdout))
          err (and stderr (make-instance 'iolib.streams:dual-channel-gray-stream :fd stderr)))))

(defmethod close ((process process) &key abort)
  (with-slots (pid reaped stdin stdout stderr)
      process
    (when stdin  (close stdin  :abort abort))
    (when stdout (close stdout :abort abort))
    (when stderr (close stderr :abort abort))
    (unless reaped
      (isys:waitpid pid (if abort isys:wnohang 0)))
    (setf pid nil stdin nil stdout nil stderr nil)))

(defmethod print-object ((o process) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S ~S" :pid (process-pid o))))

(defmacro with-lfp-spawn-arguments ((attributes file-actions pid) &body body)
  (with-gensyms (spawnattr-initialized-p file-actions-initialized-p)
    `(with-foreign-objects ((,attributes 'lfp-spawnattr-t)
                            (,file-actions 'lfp-spawn-file-actions-t)
                            (,pid 'pid-t))
       (let (,spawnattr-initialized-p
             ,file-actions-initialized-p)
         (unwind-protect
              (progn
                (lfp-spawnattr-init ,attributes)
                (setf ,spawnattr-initialized-p t)
                (lfp-spawn-file-actions-init ,file-actions)
                (setf ,file-actions-initialized-p t)
                ,@body)
           (when ,spawnattr-initialized-p
             (lfp-spawnattr-destroy ,attributes))
           (when ,file-actions-initialized-p
             (lfp-spawn-file-actions-destroy ,file-actions)))))))

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

(defun find-program (program)
  (cond
    ((eql :shell program)
     "/bin/sh")
    (t
     (file-path-namestring program))))

(defmacro with-argv (((arg0 argv) program arguments) &body body)
  (with-gensyms (argc)
    `(let ((,program (find-program ,program))
           (,argc (+ 2 (length ,arguments))))
       (with-foreign-object (,argv :pointer ,argc)
         (unwind-protect
              (progn
                (allocate-argv ,argv ,argc ,program ,arguments)
                (let ((,arg0 (mem-ref ,argv :pointer)))
                  ,@body))
           (deallocate-null-ended-list ,argv))))))

(defun redirect-one-stream (file-actions fd stream &optional flags (mode #o644) close-old-fd)
  (flet ((dup-from-path (path)
           (lfp-spawn-file-actions-addopen file-actions fd path flags mode))
         (dup-from-fd (oldfd)
           (lfp-spawn-file-actions-adddup2 file-actions oldfd fd)
           (when close-old-fd
             (lfp-spawn-file-actions-addclose file-actions oldfd))))
    (etypecase stream
      ((eql t) nil)
      ((or string file-path pathname)
       (dup-from-path (file-path-namestring stream)))
      ((eql :null)
       (dup-from-path "/dev/null"))
      (unsigned-byte
       (dup-from-fd stream))
      (iolib.streams:dual-channel-fd-mixin
       (dup-from-fd (iolib.streams:fd-of stream)))
      (null
       (lfp-spawn-file-actions-addclose file-actions fd)))))

(defun redirect-to-pipes (file-actions fd keep-write-fd)
  (multiple-value-bind (pipe-parent pipe-child)
      (isys:pipe)
    (when keep-write-fd (rotatef pipe-parent pipe-child))
    (lfp-spawn-file-actions-adddup2 file-actions pipe-child fd)
    (lfp-spawn-file-actions-addclose file-actions pipe-parent)
    (lfp-spawn-file-actions-addclose file-actions pipe-child)
    (values pipe-parent pipe-child)))

(defun setup-redirections (file-actions stdin stdout stderr)
  (let (infd infd-child outfd outfd-child errfd errfd-child)
    ;; Standard input
    (if (eql :pipe stdin)
        (setf (values infd infd-child)
              (redirect-to-pipes file-actions +stdin+ t))
        (redirect-one-stream file-actions +stdin+ stdin isys:o-rdonly))
    ;; Standard output
    (if (eql :pipe stdout)
        (setf (values outfd outfd-child)
              (redirect-to-pipes file-actions +stdout+ nil))
        (redirect-one-stream file-actions +stdout+ stdout (logior isys:o-wronly
                                                                  isys:o-creat)))
    ;; Standard error
    (cond
      ((and stdout (eql :stdout stderr))
       (redirect-one-stream file-actions +stderr+ +stdout+))
      ((eql :pipe stderr)
       (setf (values errfd errfd-child)
             (redirect-to-pipes file-actions +stderr+ nil)))
      (t
       (redirect-one-stream file-actions +stderr+ stderr (logior isys:o-wronly
                                                                 isys:o-creat))))
    (values infd infd-child outfd outfd-child errfd errfd-child)))

(defun close-fds (&rest fds)
  (dolist (fd fds)
    (when fd (isys:close fd))))

(defmacro with-redirections (((infd outfd errfd) (file-actions stdin stdout stderr))
                             &body body)
  (with-gensyms (infd-child outfd-child errfd-child)
    `(multiple-value-bind (,infd ,infd-child ,outfd ,outfd-child ,errfd ,errfd-child)
         (setup-redirections ,file-actions ,stdin ,stdout ,stderr)
       (unwind-protect-case ()
           (locally ,@body)
         (:always
          (close-fds ,infd-child ,outfd-child ,errfd-child))
         (:abort
          (close-fds ,infd ,outfd ,errfd))))))

;; program: :shell - the system shell
;;          file-path designator - a path
;; arguments: list
;; search: boolean. whether or not to search PROGRAM in $PATH when PROGRAM only names a file,
;;                  i.e., it's a relative path whose directory is NIL
;; environment: t - inherit environment
;;              nil - NULL environment
;;              alist - the environment to use
;; stdin, stdout, stderr:
;;         file-path designator - open file, redirect to it
;;         :null - redirect to /dev/null - useful because /dev/null doesn't exist on Windows
;;         file-descriptor designator(integer or stream) - file descriptor, redirecto to it
;;         :pipe - create pipe, redirect the child descriptor to one end and wrap the other end
;;                 into a stream which goes into PROCESS slot
;;         t - inherit
;;         nil - close
;; stderr: :stdout - the same as stdout

(defun create-process (program arguments &key (search t) (environment t)
                       (stdin t) (stdout t) (stderr t)
                       ;; path uid gid effective
                       )
  (with-lfp-spawn-arguments (attributes file-actions pid)
    (with-argv ((arg0 argv) program arguments)
      (with-c-environment (envp environment)
        (with-redirections ((infd outfd errfd) (file-actions stdin stdout stderr))
          (if search
              (lfp-spawnp pid arg0 argv envp file-actions attributes)
              (lfp-spawn  pid arg0 argv envp file-actions attributes))
          (make-instance 'process :pid (mem-ref pid 'pid-t)
                         :stdin infd :stdout outfd :stderr errfd))))))

(defun run-program (program &optional arguments &key (search t) environment stderr)
  (check-type stderr (member nil :stdout))
  (flet ((slurp-stream-into-string (stream)
           (with-output-to-string (s)
             (loop :for c := (read-char stream nil nil)
                   :while c :do (write-char c s)))))
    (let ((process (create-process program arguments
                                   :search search
                                   :environment environment
                                   :stdout :pipe
                                   :stderr (if (eql :stdout stderr)
                                               :stdout
                                               :pipe))))
      (unwind-protect
           (values (process-wait process)
                   (slurp-stream-into-string (process-stdout process))
                   (if (eql :stdout stderr)
                       nil
                       (slurp-stream-into-string (process-stderr process))))
        (close process)))))

(defmethod process-wait ((process process))
  (prog1
      (nth-value 1 (isys:waitpid (process-pid process) 0))
    (setf (slot-value process 'reaped) t)))

(defmethod process-kill ((process process) &optional (signum :sigterm))
  (isys:kill (process-pid process) signum))
