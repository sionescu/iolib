;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Socket creation.
;;;

(in-package :iolib.sockets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *socket-type-map*
    '(((:ipv4  :stream   :active  :default) . socket-stream-internet-active)
      ((:ipv6  :stream   :active  :default) . socket-stream-internet-active)
      ((:ipv4  :stream   :passive :default) . socket-stream-internet-passive)
      ((:ipv6  :stream   :passive :default) . socket-stream-internet-passive)
      ((:local :stream   :active  :default) . socket-stream-local-active)
      ((:local :stream   :passive :default) . socket-stream-local-passive)
      ((:local :datagram :active  :default) . socket-datagram-local-active)
      ((:ipv4  :datagram :active  :default) . socket-datagram-internet-active)
      ((:ipv6  :datagram :active  :default) . socket-datagram-internet-active)))

  ;; FIXME: should match :default to whatever protocol is the default.
  (defun select-socket-class (address-family type connect protocol)
    (or (cdr (assoc (list address-family type connect protocol) *socket-type-map*
                    :test #'equal))
        (error "No socket class found !!"))))

(defun create-socket (family type connect external-format &key
                      fd input-buffer-size output-buffer-size)
  (make-instance (select-socket-class family type connect :default)
                 :address-family family :file-descriptor fd
                 :external-format external-format
                 :input-buffer-size input-buffer-size
                 :output-buffer-size output-buffer-size))

(define-compiler-macro create-socket (&whole form &environment env
                                      family type connect external-format
                                      &key fd input-buffer-size output-buffer-size)
  (cond
    ((and (constantp family env) (constantp type env) (constantp connect env))
     `(make-instance ',(select-socket-class family type connect :default)
                     :address-family ,family :file-descriptor ,fd
                     :external-format ,external-format
                     :input-buffer-size ,input-buffer-size
                     :output-buffer-size ,output-buffer-size))
    (t form)))

(defmacro with-close-on-error ((var value) &body body)
  "Bind `VAR' to `VALUE' and execute `BODY' as implicit PROGN.
If a non-local exit occurs during the execution of `BODY',
call CLOSE with :ABORT T on `VAR'."
  `(let ((,var ,value))
     (unwind-protect-case () ,@body
       (:abort (close ,var :abort t)))))

(defmacro %create-internet-socket (family &rest args)
  `(case ,family
     (:ipv4 (create-socket :ipv4 ,@args))
     (:ipv6 (create-socket :ipv6 ,@args))))

(defmacro with-guard-against-non-list-args-and-destructuring-bind-errors
    (form args &body body)
  `(if (listp ,args)
       (handler-case (progn ,@body)
         (error (err) `(error ,err)))
       ,form))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-first-level-name (family type connect)
    (format-symbol :iolib.sockets "%~A-~A-~A-~A-~A" :make family type connect :socket)))

(defmacro define-socket-creator ((socket-family socket-type socket-connect)
                                 (family ef key &rest args) &body body)
  (assert (eql '&key key))
  (flet ((maybe-quote-default-value (arg)
           (cond ((symbolp arg) arg)
                 ((consp arg)   (list (first arg) `(quote ,(second arg))))))
         (arg-name (arg)
           (cond ((symbolp arg) arg)
                 ((consp arg)   (first arg))))
         (quotify (form)
           `(list (quote ,(car form)) ,@(cdr form))))
    (let* ((arg-names (mapcar #'arg-name args))
           (first-level-function (make-first-level-name socket-family socket-type socket-connect))
           (second-level-function (format-symbol t "%~A" first-level-function))
           (first-level-body `(,second-level-function family ef ,@arg-names)))
      `(progn
         (declaim (inline ,second-level-function))
         (defun ,second-level-function (,family ,ef ,@arg-names) ,@body)
         (defun ,first-level-function (arguments family ef)
           (destructuring-bind (&key ,@args) arguments ,first-level-body))
         (define-compiler-macro ,first-level-function (&whole form arguments family ef)
           (with-guard-against-non-list-args-and-destructuring-bind-errors
               form arguments
             ;; Must quote default values in order for them not to be evaluated
             ;; in the compilation environment
             (destructuring-bind (&key ,@(mapcar #'maybe-quote-default-value args))
                 (cdr arguments)
               ,(quotify first-level-body))))))))

;;; Internet Stream Active Socket creation

(defun %%init-internet-stream-active-socket (socket keepalive nodelay reuse-address
                                             local-host local-port remote-host remote-port)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when keepalive (setf (socket-option socket :keep-alive) t))
  (when nodelay (setf (socket-option socket :tcp-nodelay) t))
  (when local-host
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address))
  (when remote-host
    (connect socket (ensure-hostname remote-host)
             :port remote-port))
  (values socket))

(define-socket-creator (:internet :stream :active)
    (family ef &key keepalive nodelay (reuse-address t)
            local-host local-port remote-host remote-port
            input-buffer-size output-buffer-size)
  (with-close-on-error (socket (%create-internet-socket family :stream :active ef
                                                        :input-buffer-size input-buffer-size
                                                        :output-buffer-size output-buffer-size))
    (%%init-internet-stream-active-socket socket keepalive nodelay reuse-address
                                          local-host (or local-port 0) remote-host remote-port)))

;;; Internet Stream Passive Socket creation

(defun %%init-internet-stream-passive-socket (socket interface reuse-address
                                              local-host local-port backlog)
  (when local-host
    (when interface
      (setf (socket-option socket :bind-to-device) interface))
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address)
    (listen-on socket :backlog backlog))
  (values socket))

(define-socket-creator (:internet :stream :passive)
    (family ef &key interface (reuse-address t)
            local-host local-port backlog)
  (with-close-on-error (socket (%create-internet-socket family :stream :passive ef))
    (%%init-internet-stream-passive-socket socket interface reuse-address
                                           local-host (or local-port 0)
                                           (or backlog *default-backlog-size*))))

;;; Local Stream Active Socket creation

(defun %%init-local-stream-active-socket (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local)))
  (values socket))

(define-socket-creator (:local :stream :active)
    (family ef &key local-filename remote-filename
            input-buffer-size output-buffer-size)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :stream :active ef
                                              :input-buffer-size input-buffer-size
                                              :output-buffer-size output-buffer-size))
    (%%init-local-stream-active-socket socket local-filename remote-filename)))

;;; Local Stream Passive Socket creation

(defun %%init-local-stream-passive-socket (socket local-filename reuse-address backlog)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)
                  :reuse-address reuse-address)
    (listen-on socket :backlog backlog))
  (values socket))

(define-socket-creator (:local :stream :passive)
    (family ef &key local-filename (reuse-address t) backlog)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :stream :passive ef))
    (%%init-local-stream-passive-socket socket local-filename reuse-address
                                        (or backlog *default-backlog-size*))))

;;; Internet Datagram Socket creation

(defun %%init-internet-datagram-active-socket (socket broadcast interface reuse-address
                                               local-host local-port remote-host remote-port)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when broadcast (setf (socket-option socket :broadcast) t))
  (when local-host
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address)
    (when interface
      (setf (socket-option socket :bind-to-device) interface)))
  (when remote-host
    (connect socket (ensure-hostname remote-host)
             :port remote-port))
  (values socket))

(define-socket-creator (:internet :datagram :active)
    (family ef &key broadcast interface (reuse-address t)
            local-host local-port remote-host remote-port)
  (with-close-on-error (socket (%create-internet-socket family :datagram :active ef))
    (%%init-internet-datagram-active-socket socket broadcast interface reuse-address
                                            local-host (or local-port 0)
                                            remote-host (or remote-port 0))))

;;; Local Datagram Socket creation

(defun %%init-local-datagram-active-socket (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local)))
  (values socket))

(define-socket-creator (:local :datagram :active)
    (family ef &key local-filename remote-filename)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :datagram :active ef))
    (%%init-local-datagram-active-socket socket local-filename remote-filename)))

;;; MAKE-SOCKET

(defmethod make-socket (&rest args &key (address-family :internet) (type :stream)
                        (connect :active) (ipv6 *ipv6*)
                        (external-format :default) &allow-other-keys)
  (when (eql :file address-family) (setf address-family :local))
  (check-type address-family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL(or :FILE), :IPV4 or :IPV6")
  (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-from-plist args :address-family :type :connect :external-format :ipv6)))
    (when (eql :ipv4 address-family) (setf ipv6 nil))
    (let ((*ipv6* ipv6))
      (when (eql :internet address-family) (setf address-family +default-inet-address-family+))
      (multiple-value-case ((address-family type connect))
        (((:ipv4 :ipv6) :stream :active)
         (%make-internet-stream-active-socket args address-family external-format))
        (((:ipv4 :ipv6) :stream :passive)
         (%make-internet-stream-passive-socket args address-family external-format))
        ((:local :stream :active)
         (%make-local-stream-active-socket args :local external-format))
        ((:local :stream :passive)
         (%make-local-stream-passive-socket args :local external-format))
        (((:ipv4 :ipv6) :datagram)
         (%make-internet-datagram-active-socket args address-family external-format))
        ((:local :datagram)
         (%make-local-datagram-active-socket args :local external-format))))))

(define-compiler-macro make-socket (&whole form &environment env &rest args
                                    &key (address-family :internet) (type :stream)
                                    (connect :active) (ipv6 '*ipv6* ipv6p)
                                    (external-format :default) &allow-other-keys)
  (when (eql :file address-family) (setf address-family :local))
  (cond
    ((and (constantp address-family env) (constantp type env) (constantp connect env))
     (check-type address-family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL(or :FILE), :IPV4 or :IPV6")
     (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
     (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
     (let* ((family (if (member address-family '(:ipv4 :ipv6)) :internet address-family))
            (lower-function (make-first-level-name family type connect))
            (newargs (remove-from-plist args :address-family :type :connect :external-format :ipv6)))
       (case address-family
         (:internet (setf address-family '+default-inet-address-family+))
         (:ipv4     (setf ipv6 nil ipv6p t)))
       (let ((expansion `(,lower-function (list ,@newargs) ,address-family ,external-format)))
         (if ipv6p `(let ((*ipv6* ,ipv6)) ,expansion) expansion))))
    (t form)))

(defmacro with-open-socket ((var &rest args) &body body)
  "Bind VAR to a socket created by passing ARGS to MAKE-SOCKET and execute BODY as implicit PROGN.
The socket is automatically closed upon exit."
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

(defmacro with-accept-connection ((var passive-socket &rest args) &body body)
  "Bind VAR to a socket created by passing PASSIVE-SOCKET and ARGS to ACCEPT-CONNECTION and execute BODY as implicit PROGN.
The socket is automatically closed upon exit."
  `(with-open-stream (,var (accept-connection ,passive-socket ,@args)) ,@body))

;;; MAKE-SOCKET-FROM-FD

;;; FIXME: must come up with a way to find out
;;; whether a socket is active or passive
(defmethod make-socket-from-fd ((fd integer) &key (connect :active) (external-format :default)
                                input-buffer-size output-buffer-size)
  (flet ((%get-address-family (fd)
           (with-sockaddr-storage-and-socklen (ss size)
             (%getsockname fd ss size)
             (eswitch ((foreign-slot-value ss 'sockaddr-storage 'family) :test #'=)
               (af-inet  :ipv4)
               (af-inet6 :ipv6)
               (af-local :local))))
         (%get-type (fd)
           (eswitch ((get-socket-option-int fd sol-socket so-type) :test #'=)
             (sock-stream :stream)
             (sock-dgram  :datagram))))
    (create-socket (%get-address-family fd)
                   (%get-type fd)
                   connect external-format :fd fd
                   :input-buffer-size input-buffer-size
                   :output-buffer-size output-buffer-size)))

;;; MAKE-SOCKET-PAIR

(defmethod make-socket-pair (&key (type :stream) (protocol :default) (external-format :default)
                             input-buffer-size output-buffer-size)
  (flet ((%make-socket-pair (fd)
           (make-socket-from-fd fd :external-format external-format
                                :input-buffer-size input-buffer-size
                                :output-buffer-size output-buffer-size)))
    (multiple-value-bind (fd1 fd2)
        (multiple-value-call #'%socketpair
          (translate-make-socket-keywords-to-constants :local type protocol))
      (values (%make-socket-pair fd1)
              (%make-socket-pair fd2)))))

;;; SEND/RECEIVE-FILE-DESCRIPTOR

(defun call-with-buffers-for-fd-passing (fn)
  (with-foreign-object (msg 'msghdr)
    (isys:bzero msg size-of-msghdr)
    (with-foreign-pointer (buffer #.(isys:cmsg.space size-of-int) buffer-size)
      (isys:bzero buffer buffer-size)
      (with-foreign-slots ((control controllen) msg msghdr)
        (setf control    buffer
              controllen buffer-size)
        (let ((cmsg (isys:cmsg.firsthdr msg)))
          (with-foreign-slots ((len level type) cmsg cmsghdr)
            (setf len (isys:cmsg.len size-of-int)
                  level sol-socket
                  type scm-rights)
            (funcall fn msg cmsg)))))))

(defmacro with-buffers-for-fd-passing ((msg-var cmsg-var) &body body)
  `(call-with-buffers-for-fd-passing (lambda (,msg-var ,cmsg-var) ,@body)))

(defmethod send-file-descriptor ((socket local-socket) file-descriptor)
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (isys:cmsg.data cmsg)))
      (setf (mem-aref data :int) file-descriptor)
      (%sendmsg (fd-of socket) msg 0)
      (values))))

(defmethod receive-file-descriptor ((socket local-socket))
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (isys:cmsg.data cmsg)))
      (%recvmsg (fd-of socket) msg 0)
      (mem-aref data :int))))
