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

(define-socket-call ("accept" %accept) :int
  "Accept an incoming connection, returning the file descriptor."
  (socket  fd)
  (address :pointer) ; sockaddr-foo
  (addrlen :pointer))

(define-socket-call ("bind" %bind) :int
  "Bind a socket to a particular local address."
  (fd      fd)
  (address :pointer)
  (addrlen socklen))

(define-socket-call ("connect" %connect) :int
  "Create an outgoing connection on a given socket."
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
  "Retrieve socket configuration."
  (fd      fd)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  :pointer))

(define-socket-call ("listen" %listen) :int
  "Mark a bound socket as listening for incoming connections."
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

#-(and) ; unused
(define-socket-call ("recvmsg" %recvmsg) ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

#-(and) ; unused
(define-socket-call ("sendmsg" %sendmsg) ssize
  (socket  fd)
  (message :pointer)
  (flags   :int))

(define-socket-call ("setsockopt" %setsockopt) :int
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
(define-socket-call ("socket" %socket) :int
  "Create a BSD socket."
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int))

#-(and) ; unused
(define-socket-call ("sockatmark" %sockatmark) :int
  (socket fd))

#-(and) ; unused
(define-socket-call ("socketpair" %%socketpair) :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int)
  (filedes  :pointer))

#-(and) ; unused
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
    (errno-wrapper :unsigned-int :error-predicate zerop)
  (ifname :string))

(define-socket-call ("if_indextoname" %if-indextoname) :string
  (ifindex :unsigned-int)
  (ifname  :pointer))

(define-socket-call ("if_nameindex" %if-nameindex) :pointer
  "Return all network interface names and indexes")

(define-socket-call ("if_freenameindex" %if-freenameindex) :void
  (ptr :pointer))
