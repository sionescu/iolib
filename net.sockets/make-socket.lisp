;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Socket creation.
;;;

(in-package :net.sockets)

(defun create-socket (family type connect external-format &key
                      fd input-buffer-size output-buffer-size)
  (make-instance (select-socket-class family type connect :default)
                 :address-family family :file-descriptor fd
                 :external-format external-format
                 :input-buffer-size input-buffer-size
                 :output-buffer-size output-buffer-size))

(define-compiler-macro create-socket (&whole form family type connect external-format
                                      &key fd input-buffer-size output-buffer-size)
  (cond
    ((and (constantp family) (constantp type) (constantp connect))
     `(make-instance ',(select-socket-class family type connect :default)
                     :address-family ,family :file-descriptor ,fd
                     :external-format ,external-format
                     :input-buffer-size ,input-buffer-size
                     :output-buffer-size ,output-buffer-size))
    (t form)))

(defmacro with-close-on-error ((var value) &body body)
  "Bind `VAR' to `VALUE', execute `BODY' as implicit PROGN and return `VAR'.
If a non-local exit occurs during the execution of `BODY' call CLOSE with :ABORT T on `VAR'."
  (with-gensyms (errorp)
    `(let ((,var ,value) (,errorp t))
       (unwind-protect
            (multiple-value-prog1 (locally ,@body ,var) (setf ,errorp nil))
         (when (and ,var ,errorp) (close ,var :abort t))))))

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

;;; Internet Stream Active Socket creation

(defun %%init-internet-stream-active-socket (socket keepalive nodelay reuse-address
                                             local-host local-port remote-host remote-port)
  (let ((local-port  (ensure-numerical-service local-port))
        (remote-port (ensure-numerical-service remote-port)))
    (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
    (when keepalive (setf (socket-option socket :keep-alive) t))
    (when nodelay (setf (socket-option socket :tcp-nodelay) t))
    (when local-host
      (bind-address socket (ensure-hostname local-host)
                    :port local-port
                    :reuse-address reuse-address))
    (when (plusp remote-port)
      (connect socket (ensure-hostname remote-host)
               :port remote-port))))

(declaim (inline %%make-internet-stream-active-socket))
(defun %%make-internet-stream-active-socket (family ef keepalive nodelay reuse-address
                                             local-host local-port remote-host remote-port
                                             input-buffer-size output-buffer-size)
  (with-close-on-error (socket (%create-internet-socket family :stream :active ef
                                                        :input-buffer-size input-buffer-size
                                                        :output-buffer-size output-buffer-size))
    (%%init-internet-stream-active-socket socket keepalive nodelay reuse-address
                                          local-host local-port remote-host remote-port)))

(defun %make-internet-stream-active-socket (args family ef)
  (destructuring-bind (&key keepalive nodelay (reuse-address t)
                            local-host (local-port 0)
                            (remote-host +any-host+) (remote-port 0)
                            input-buffer-size output-buffer-size)
      args
    (%%make-internet-stream-active-socket family ef keepalive nodelay reuse-address
                                          local-host local-port remote-host remote-port
                                          input-buffer-size output-buffer-size)))

(define-compiler-macro %make-internet-stream-active-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key keepalive nodelay (reuse-address t)
                              local-host (local-port 0)
                              (remote-host +any-host+) (remote-port 0)
                              input-buffer-size output-buffer-size)
        (cdr args)
      `(%%make-internet-stream-active-socket ,family ,ef ,keepalive ,nodelay ,reuse-address
                                             ,local-host ,local-port ,remote-host ,remote-port
                                             ,input-buffer-size ,output-buffer-size))))

;;; Internet Stream Passive Socket creation

(defun %%init-internet-stream-passive-socket (socket interface reuse-address
                                              local-host local-port backlog)
  (let ((local-port  (ensure-numerical-service local-port)))
    (when local-host
      (when interface
        (setf (socket-option socket :bind-to-device) interface))
      (bind-address socket (ensure-hostname local-host)
                    :port local-port
                    :reuse-address reuse-address)
      (listen-on socket :backlog backlog))))

(declaim (inline %%make-internet-stream-passive-socket))
(defun %%make-internet-stream-passive-socket (family ef interface reuse-address
                                              local-host local-port backlog)
  (with-close-on-error (socket (%create-internet-socket family :stream :passive ef))
    (%%init-internet-stream-passive-socket socket interface reuse-address
                                           local-host local-port backlog)))

(defun %make-internet-stream-passive-socket (args family ef)
  (destructuring-bind (&key interface (reuse-address t)
                            (local-host +any-host+) (local-port 0)
                            (backlog *default-backlog-size*))
      args
    (%%make-internet-stream-passive-socket family ef interface reuse-address
                                           local-host local-port backlog)))

(define-compiler-macro %make-internet-stream-passive-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key interface (reuse-address t)
                              (local-host +any-host+) (local-port 0)
                              (backlog *default-backlog-size*))
        (cdr args)
      `(%%make-internet-stream-passive-socket ,family ,ef ,interface ,reuse-address
                                              ,local-host ,local-port ,backlog))))

;;; Local Stream Active Socket creation

(defun %%init-local-stream-active-socket (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local))))

(declaim (inline %%make-local-stream-active-socket))
(defun %%make-local-stream-active-socket (family ef local-filename remote-filename
                                          input-buffer-size output-buffer-size)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :stream :active ef
                                              :input-buffer-size input-buffer-size
                                              :output-buffer-size output-buffer-size))
    (%%init-local-stream-active-socket socket local-filename remote-filename)))

(defun %make-local-stream-active-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename
                            input-buffer-size output-buffer-size)
      args
    (%%make-local-stream-active-socket family ef local-filename remote-filename
                                       input-buffer-size output-buffer-size)))

(define-compiler-macro %make-local-stream-active-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename remote-filename
                              input-buffer-size output-buffer-size)
        (cdr args)
      `(%%make-local-stream-active-socket ,family ,ef ,local-filename ,remote-filename
                                          ,input-buffer-size ,output-buffer-size))))

;;; Local Stream Passive Socket creation

(defun %%init-local-stream-passive-socket (socket local-filename reuse-address backlog)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)
                  :reuse-address reuse-address)
    (listen-on socket :backlog backlog)))

(declaim (inline %%make-local-stream-passive-socket))
(defun %%make-local-stream-passive-socket (family ef local-filename reuse-address backlog)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :stream :passive ef))
    (%%init-local-stream-passive-socket socket local-filename reuse-address backlog)))

(defun %make-local-stream-passive-socket (args family ef)
  (destructuring-bind (&key local-filename (reuse-address t)
                            (backlog *default-backlog-size*))
      args
    (%%make-local-stream-passive-socket family ef local-filename reuse-address backlog)))

(define-compiler-macro %make-local-stream-passive-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename (reuse-address t)
                              (backlog *default-backlog-size*))
        (cdr args)
      `(%%make-local-stream-passive-socket ,family ,ef ,local-filename ,reuse-address ,backlog))))

;;; Internet Datagram Socket creation

(defun %%init-internet-datagram-socket (socket broadcast interface reuse-address
                                        local-host local-port remote-host remote-port)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (let ((local-port  (ensure-numerical-service local-port))
        (remote-port (ensure-numerical-service remote-port)))
    (when broadcast (setf (socket-option socket :broadcast) t))
    (when local-host
      (bind-address socket (ensure-hostname local-host)
                    :port local-port
                    :reuse-address reuse-address)
      (when interface
        (setf (socket-option socket :bind-to-device) interface)))
    (when (plusp remote-port)
      (connect socket (ensure-hostname remote-host)
               :port remote-port))))

(declaim (inline %%make-internet-datagram-socket))
(defun %%make-internet-datagram-socket (family ef broadcast interface reuse-address
                                        local-host local-port remote-host remote-port)
  (with-close-on-error (socket (%create-internet-socket family :datagram :active ef))
    (%%init-internet-datagram-socket socket broadcast interface reuse-address
                                     local-host local-port remote-host remote-port)))

(defun %make-internet-datagram-socket (args family ef)
  (destructuring-bind (&key broadcast interface (reuse-address t)
                            local-host (local-port 0)
                            (remote-host +any-host+) (remote-port 0))
      args
    (%%make-internet-datagram-socket family ef broadcast interface reuse-address
                                     local-host local-port remote-host remote-port)))

(define-compiler-macro %make-internet-datagram-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key broadcast interface (reuse-address t)
                              local-host (local-port 0)
                              (remote-host +any-host+) (remote-port 0))
        (cdr args)
      `(%%make-internet-datagram-socket ,family ,ef ,broadcast ,interface ,reuse-address
                                        ,local-host ,local-port ,remote-host ,remote-port))))

;;; Local Datagram Socket creation

(defun %%init-local-datagram-socket (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local))))

(declaim (inline %%make-local-datagram-socket))
(defun %%make-local-datagram-socket (family ef local-filename remote-filename)
  (declare (ignore family))
  (with-close-on-error (socket (create-socket :local :datagram :active ef))
    (%%init-local-datagram-socket socket local-filename remote-filename)))

(defun %make-local-datagram-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename)
      args
    (%%make-local-datagram-socket family ef local-filename remote-filename)))

(define-compiler-macro %make-local-datagram-socket (&whole form args family ef)
  (with-guard-against-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename remote-filename)
        (cdr args)
      `(%%make-local-datagram-socket ,family ,ef ,local-filename ,remote-filename))))

;;; MAKE-SOCKET

(defun make-socket (&rest args &key (address-family :internet) (type :stream)
                    (connect :active) (ipv6 *ipv6*)
                    (external-format :default) &allow-other-keys)
  "Creates a socket instance of the appropriate subclass of SOCKET."
  (check-type address-family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL, :IPV4 or :IPV6")
  (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-from-plist args :address-family :type :connect :external-format :ipv6)))
    (when (eq :ipv4 address-family) (setf ipv6 nil))
    (let ((*ipv6* ipv6))
      (when (eq :internet address-family) (setf address-family +default-inet-address-family+))
      (multiple-value-case ((address-family type connect) :test #'eq)
        (((:ipv4 :ipv6) :stream :active)
         (%make-internet-stream-active-socket args address-family external-format))
        (((:ipv4 :ipv6) :stream :passive)
         (%make-internet-stream-passive-socket args address-family external-format))
        ((:local :stream :active)
         (%make-local-stream-active-socket args :local external-format))
        ((:local :stream :passive)
         (%make-local-stream-passive-socket args :local external-format))
        (((:ipv4 :ipv6) :datagram)
         (%make-internet-datagram-socket args address-family external-format))
        ((:local :datagram)
         (%make-local-datagram-socket args :local external-format))))))

(define-compiler-macro make-socket (&whole form &rest args &key (address-family :internet) (type :stream)
                                    (connect :active) (ipv6 '*ipv6* ipv6p)
                                    (external-format :default) &allow-other-keys)
  (cond
    ((and (constantp address-family) (constantp type) (constantp connect))
     (check-type address-family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL, :IPV4 or :IPV6")
     (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
     (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
     (let ((lower-function
            (multiple-value-case ((address-family type connect) :test #'eq)
              (((:ipv4 :ipv6 :internet) :stream :active) '%make-internet-stream-active-socket)
              (((:ipv4 :ipv6 :internet) :stream :passive) '%make-internet-stream-passive-socket)
              ((:local :stream :active) '%make-local-stream-active-socket)
              ((:local :stream :passive) '%make-local-stream-passive-socket)
              (((:ipv4 :ipv6 :internet) :datagram) '%make-internet-datagram-socket)
              ((:local :datagram) '%make-local-datagram-socket)))
           (newargs (remove-from-plist args :address-family :type :connect :external-format :ipv6)))
       (multiple-value-case (address-family)
         (:internet (setf address-family '+default-inet-address-family+))
         (:ipv4     (setf ipv6 nil ipv6p t)))
       (let ((expansion `(,lower-function (list ,@newargs) ,address-family ,external-format)))
         (if ipv6p `(let ((*ipv6* ,ipv6)) ,expansion) expansion))))
    (t form)))

(defmacro with-open-socket ((var &rest args) &body body)
  "VAR is bound to a socket created by passing ARGS to
MAKE-SOCKET and BODY is executed as implicit PROGN.  The socket
is automatically closed upon exit."
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

(defmacro with-accept-connection ((var passive-socket &rest args) &body body)
  "VAR is bound to a socket created by passing PASSIVE-SOCKET and ARGS to
ACCEPT-CONNECTION and BODY is executed as implicit PROGN.  The socket
is automatically closed upon exit."
  `(with-open-stream (,var (accept-connection ,passive-socket ,@args)) ,@body))

;;; MAKE-SOCKET-FROM-FD

;;; FIXME: must come up with a way to find out
;;; whether a socket is active or passive
(defun make-socket-from-fd (fd &key (connect :active) (external-format :default)
                            input-buffer-size output-buffer-size)
  "Creates an socket instance of the appropriate subclass of SOCKET using `FD'.
The connection type of the socket must be specified(:ACTIVE or :PASSIVE).
The address family and type of the socket is automatically discovered using OS functions. Buffer sizes
for the new socket can also be specified using `INPUT-BUFFER-SIZE' and `OUTPUT-BUFFER-SIZE'."
  (flet ((%get-address-family (fd)
           (with-sockaddr-storage-and-socklen (ss size)
             (%getsockname fd ss size)
             (foreign-slot-value ss 'sockaddr-storage 'family)
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

(defun make-socket-pair (&key (type :stream) (protocol :default) (external-format :default)
                         input-buffer-size output-buffer-size)
  "Creates a pair of sockets connected to each other.
The socket type can be either :STREAM or :DATAGRAM. Currently OSes can only create :LOCAL sockets this way.
Buffer sizes for the new sockets can also be specified using `INPUT-BUFFER-SIZE' and `OUTPUT-BUFFER-SIZE'."
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
    (bzero msg size-of-msghdr)
    (with-foreign-pointer (buffer (%cmsg-space size-of-int) buffer-size)
      (bzero buffer buffer-size)
      (with-foreign-slots ((control controllen) msg msghdr)
        (setf control    buffer
              controllen buffer-size)
        (let ((cmsg (%cmsg-firsthdr msg)))
          (with-foreign-slots ((len level type) cmsg cmsghdr)
            (setf len (%cmsg-len size-of-int)
                  level sol-socket
                  type scm-rights)
            (funcall fn msg cmsg)))))))

(defmacro with-buffers-for-fd-passing ((msg-var cmsg-var) &body body)
  `(call-with-buffers-for-fd-passing #'(lambda (,msg-var ,cmsg-var) ,@body)))

(defmethod send-file-descriptor ((socket local-socket) file-descriptor)
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (%cmsg-data cmsg)))
      (setf (mem-ref data :int) file-descriptor)
      (%sendmsg (fd-of socket) msg 0)
      (values))))

(defmethod receive-file-descriptor ((socket local-socket))
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (%cmsg-data cmsg)))
      (%recvmsg (fd-of socket) msg 0)
      (mem-ref data :int))))
