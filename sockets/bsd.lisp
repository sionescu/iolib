;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; bsd.lisp --- Bindings for BSD sockets.
;;;
;;; Copyright (C) 2005-2006, Matthew Backes  <lucca@accela.net>
;;; Copyright (C) 2005-2006, Dan Knapp  <dankna@accela.net> and
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :net.sockets)

(defmacro deforeign (name-and-opts return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name-and-opts)
    `(defcfun (,c-name ,lisp-name ,@options) ,return-type
       ,@args)))

(defmacro define-socket-call (name return-type &body args)
  `(deforeign ,name (errno-wrapper ,return-type
                                   :error-generator signal-socket-error)
     ,@args))

(defctype fd :int)


;;;; sys/socket.h

(define-socket-call "accept" :int
  "Accept an incoming connection, returning the file descriptor."
  (socket  fd)
  (address :pointer) ; sockaddr-foo
  (addrlen :pointer))

(define-socket-call "bind" :int
  "Bind a socket to a particular local address."
  (fd      fd)
  (address :pointer)
  (addrlen socklen))

(define-socket-call ("connect" %connect) :int
  "Create an outgoing connection on a given socket."
  (socket  fd)
  (address :pointer) ; sockaddr-foo
  (addrlen socklen))

(define-socket-call "getpeername" :int
  (socket  fd)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call "getsockname" :int
  (socket  fd)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call "getsockopt" :int
  "Retrieve socket configuration."
  (fd      fd)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  :pointer))

(define-socket-call "listen" :int
  "Mark a bound socket as listening for incoming connections."
  (socket  fd)
  (backlog :int))

(define-socket-call "recv" ssize
  (socket fd)
  (buffer :pointer)
  (length size)
  (flags  :int))

(define-socket-call "recvfrom" ssize
  (socket  fd)
  (buffer  :pointer)
  (length  size)
  (flags   :int)
  (address :pointer)
  (addrlen :pointer))

#-(and) ; unused
(define-socket-call "recvmsg" ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

(define-socket-call "send" ssize
  (socket fd)
  (buffer :pointer)
  (length size)
  (flags  :int))

#-(and) ; unused
(define-socket-call "sendmsg" ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

(define-socket-call "sendto" ssize
  (socket   fd)
  (buffer   :pointer)
  (length   size)
  (flags    :int)
  (destaddr :pointer)
  (destlen  socklen))

(define-socket-call "setsockopt" :int
  "Configure a socket."
  (fd      fd)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  socklen))

(define-socket-call ("shutdown" %shutdown) :int
  (socket fd)
  (how    :int))

;;; SOCKET is emulated in winsock.lisp.
(define-socket-call "socket" :int
  "Create a BSD socket."
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int))

#-(and) ; unused
(define-socket-call "sockatmark" :int
  (socket fd))

#-(and) ; unused
(define-socket-call ("socketpair" %socketpair) :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int)
  (filedes  :pointer))

#-(and) ; unused
(defun socketpair (domain type protocol)
  (with-foreign-object (filedes :int 2)
    (%socketpair domain type protocol filedes)
    (values (mem-aref filedes :int 0)
            (mem-aref filedes :int 1))))

;;;; netinet/un.h

(defconstant unix-path-max
  (- size-of-sockaddr-un (foreign-slot-offset 'sockaddr-un 'path)))

;;;; netdb.h

;;; TODO: more socket stuff, deal with this later

(define-socket-call "freeaddrinfo" :void
  (ai :pointer))

(defcfun "getaddrinfo"
    (errno-wrapper :int
                   :error-predicate (lambda (x) (not (zerop x)))
                   :error-generator signal-resolver-error)
  (node    :string)
  (service :string)
  (hints   :pointer)
  (result  :pointer))

;;; For systems with missing or broken getaddrinfo().
(defcfun "gethostbyaddr" hostent
  (addr :pointer)
  (len  socklen)
  (type :int))

;;; ditto
(defcfun "getservbyport" servent
  (port  :int)
  (proto :string))

(defcfun "getnameinfo"
    (errno-wrapper :int
                   :error-predicate (lambda (x) (not (zerop x)))
                   :error-generator signal-resolver-error)
  (sa         :pointer)
  (salen      socklen)
  (node       :pointer)
  (nodelen    socklen)
  (service    :pointer)
  (servicelen socklen)
  (flags      :int))

(define-socket-call "getprotobyname" :pointer
  (name :string))

(define-socket-call "getprotobynumber" :pointer
  (proto :int))

;;;; arpa/inet.h

#-(and) ; unused
(define-socket-call "inet_ntop" :string
  (family :int)
  (src    :pointer)
  (dest   :pointer)
  (size   socklen))

(defcfun "inet_pton"
    (errno-wrapper :int :error-predicate (lambda (x) (not (plusp x))))
  (family :int)
  (src    :string)
  (dest   :pointer))

;;;; net/if.h

(defcfun "if_nametoindex"
    (errno-wrapper :unsigned-int :error-predicate zerop)
  (ifname :string))

(define-socket-call "if_indextoname" :string
  (ifindex :unsigned-int)
  (ifname  :pointer))

(define-socket-call "if_nameindex" :pointer
  "Return all network interface names and indexes")

(define-socket-call "if_freenameindex" :void
  (ptr :pointer))
