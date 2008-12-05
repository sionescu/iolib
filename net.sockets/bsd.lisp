;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Bindings for BSD sockets.
;;;

(in-package :net.sockets)

(defmacro deforeign (name-and-opts return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name-and-opts)
    `(defcfun (,c-name ,lisp-name ,@options) ,return-type
       ,@args)))

(defmacro define-socket-creation-call (name return-type &body args)
  `(deforeign ,name (errno-wrapper ,return-type
                                   :error-generator signal-socket-error)
     ,@args))

(defmacro define-socket-call (name return-type &body args)
  (let ((forms (alexandria:parse-body args)))
    `(deforeign ,name (errno-wrapper ,return-type
                                     :object ,(caar forms) ; the socket FD
                                     :error-generator signal-socket-error)
       ,@forms)))

(defctype fd :int)


;;;; sys/socket.h

(define-socket-call ("accept" %accept) :int
  (socket  fd)
  (address :pointer) ; sockaddr-foo
  (addrlen :pointer))

(define-socket-call ("bind" %bind) :int
  (socket  fd)
  (address :pointer)
  (addrlen socklen))

(define-socket-call ("connect" %connect) :int
  (socket  fd)
  (address :pointer) ; sockaddr-foo
  (addrlen socklen))

(define-socket-call ("getpeername" %getpeername) :int
  (socket  fd)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call ("getsockname" %getsockname) :int
  (socket  fd)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call ("getsockopt" %getsockopt) :int
  (socket  fd)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  :pointer))

(define-socket-call ("listen" %listen) :int
  (socket  fd)
  (backlog :int))

(define-socket-call ("recvfrom" %recvfrom) ssize
  (socket  fd)
  (buffer  :pointer)
  (length  size)
  (flags   :int)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call ("sendto" %sendto) ssize
  (socket   fd)
  (buffer   :pointer)
  (length   size)
  (flags    :int)
  (destaddr :pointer)
  (destlen  socklen))

(define-socket-call ("recvmsg" %recvmsg) ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

(define-socket-call ("sendmsg" %sendmsg) ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

(define-socket-call ("setsockopt" %setsockopt) :int
  (socket  fd)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  socklen))

(define-socket-call ("shutdown" %shutdown) :int
  (socket fd)
  (how    :int))

;;; SOCKET is emulated in winsock.lisp.
(define-socket-creation-call ("socket" %socket) :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int))

#-(and) ; unused
(define-socket-call ("sockatmark" %sockatmark) :int
  (socket fd))

(define-socket-creation-call ("socketpair" %%socketpair) :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int)  ; usually 0 - "default protocol", whatever that is
  (filedes  :pointer))

(defun %socketpair (domain type protocol)
  (with-foreign-object (filedes :int 2)
    (%%socketpair domain type protocol filedes)
    (values (mem-aref filedes :int 0)
            (mem-aref filedes :int 1))))

;;;; netinet/un.h

(defconstant unix-path-max
  (- size-of-sockaddr-un (foreign-slot-offset 'sockaddr-un 'path)))

;;;; net/if.h

(defcfun ("if_nametoindex" %if-nametoindex)
    (errno-wrapper :unsigned-int :error-predicate zerop
                   :error-generator (lambda (r o)
                                      (declare (ignore r o))
                                      (nix::posix-error :enxio)))
  (ifname :string))

(defcfun ("if_indextoname" %if-indextoname)
    (errno-wrapper :string)
  (ifindex :unsigned-int)
  (ifname  :pointer))

(defcfun ("if_nameindex" %if-nameindex)
    (errno-wrapper :pointer))

(defcfun ("if_freenameindex" %if-freenameindex) :void
  (ptr :pointer))
