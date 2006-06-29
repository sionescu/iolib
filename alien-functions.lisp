;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 2)))
;; (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(in-package #:iolib-alien)


;; size of standard types
(defconstant size-of-short (/ (alien-size short) 8))
(defconstant size-of-int (/ (alien-size int) 8))
(defconstant size-of-long (/ (alien-size long) 8))
(defconstant size-of-long-long (/ (alien-size long-long) 8))

;;
;; very ugly hack :(
;;
(defmacro grovel-enum-members (enum)
  (let ((deflist (gensym)))
    `(let (,deflist)
       (loop for (member . value)
          in (slot-value (slot-value (slot-value (make-alien ,enum)
                                                 'sb-alien::type)
                                     'sb-alien::to)
                         'sb-alien::from)
          do (push (list 'defconstant (intern (symbol-name member)) value)
                   ,deflist))
       (eval (cons 'progn ,deflist)))))


;;;;;;;;;;;;;;;;
;;;          ;;;
;;; unistd.h ;;;
;;;          ;;;
;;;;;;;;;;;;;;;;

(define-alien-routine "pread" ssize-t
  (fd int)
  (buf (* t))
  (count size-t)
  (offset off-t))

(define-alien-routine "pwrite" ssize-t
  (fd int)
  (buf (* t))
  (count size-t)
  (offset off-t))


;;;;;;;;;;;;;;;;;
;;;           ;;;
;;; sys/uio.h ;;;
;;;           ;;;
;;;;;;;;;;;;;;;;;

(define-alien-routine "readv" ssize-t
  (fd int)
  (iov (* (struct iovec)))
  (count int))

(define-alien-routine "writev" ssize-t
  (fd int)
  (iov (* (array (struct iovec) 0)))
  (count int))


;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;; sys/socket.h ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;

(define-alien-routine "accept" int
  (socket int)
  (address (* t))
  (addrlen (* socklen-t)))

(define-alien-routine "bind" int
  (socket int)
  (address (* t))
  (addrlen socklen-t))

(define-alien-routine "connect" int
  (socket int)
  (address (* t))
  (addrlen socklen-t))

(define-alien-routine "getpeername" int
  (socket int)
  (address (* t))
  (addrlen (* socklen-t)))

(define-alien-routine "getsockname" int
  (socket int)
  (address (* t))
  (addrlen (* socklen-t)))

(define-alien-routine "getsockopt" int
  (socket int)
  (level int)
  (optname int)
  (optval (* t))
  (optlen (* socklen-t)))

(define-alien-routine "listen" int
    (socket int)
    (backlog int))

(define-alien-routine "recv" ssize-t
  (socket int)
  (buffer (* t))
  (length size-t)
  (flags int))

(define-alien-routine "recvfrom" ssize-t
  (socket int)
  (buffer (* t))
  (length size-t)
  (flags int)
  (address (* t))
  (addrlen (* socklen-t)))

(define-alien-routine "recvmsg" ssize-t
  (socket int)
  (message (* (struct msghdr)))
  (flags int))

(define-alien-routine "send" ssize-t
  (socket int)
  (buffer (* t))
  (length size-t)
  (flags int))

(define-alien-routine "sendmsg" ssize-t
  (socket int)
  (message (* (struct msghdr)))
  (flags int))

(define-alien-routine "sendto" ssize-t
  (socket int)
  (message (* t))
  (length size-t)
  (flags int)
  (destaddr (* t))
  (destlen socklen-t))

(define-alien-routine "setsockopt" int
  (socket int)
  (level int)
  (optname int)
  (optval (* t))
  (optlen socklen-t))

(define-alien-routine "shutdown" int
  (socket int)
  (how int))

(define-alien-routine "socket" int
  (domain int)
  (type int)
  (protocol int))

(define-alien-routine "sockatmark" int
  (socket int))

(define-alien-routine "socketpair" int
  (domain int)
  (type int)
  (protocol int)
  (socket-vector (* (array int 2))))


;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;; netinet/un.h ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;

(defconstant unix-path-max #+linux   108
                           #+freebsd 104)


;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;; netinet/in.h ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;

(define-alien-variable "in6addr_any" (struct in6-addr))
(define-alien-variable "in6addr_loopback" (struct in6-addr))
(define-alien-variable "ipv6mr_multiaddr" (struct in6-addr))
(define-alien-variable "ipv6mr_interface" unsigned)


;;;;;;;;;;;;;;;;;;;;;
;;;               ;;;
;;; netinet/tcp.h ;;;
;;;               ;;;
;;;;;;;;;;;;;;;;;;;;;

#+linux
(grovel-enum-members connstates)

;;;;;;;;;;;;;;;
;;;         ;;;
;;; netdb.h ;;;
;;;         ;;;
;;;;;;;;;;;;;;;

(define-alien-routine "freeaddrinfo" void
  (ai (* (struct addrinfo))))

(define-alien-routine "getaddrinfo" int
  (nodename c-string)
  (servname c-string)
  (hints (* (struct addrinfo)))
  (result (* (* (struct addrinfo)))))

(define-alien-routine "getnameinfo" int
  (sa (* t))
  (salen socklen-t)
  (node (* (array char 0)))
  (nodelen socklen-t)
  (service (* (array char 0)))
  (servicelen socklen-t)
  (flags int))

(define-alien-routine "gai_strerror" c-string
  (ercode int))

(define-alien-routine "endprotoent" void)

(define-alien-routine "getprotobyname" (* (struct protoent))
  (name c-string))

(define-alien-routine "getprotobynumber" (* (struct protoent))
  (proto int))

(define-alien-routine "getprotoent" (* (struct protoent)))

(define-alien-routine "setprotoent" void
  (stayopen int))


;;;;;;;;;;;;;;;;
;;;          ;;;
;;; net/if.h ;;;
;;;          ;;;
;;;;;;;;;;;;;;;;

(define-alien-routine "if_nametoindex" unsigned
  (ifname c-string))

(define-alien-routine "if_indextoname" c-string
  (ifindex unsigned)
  (ifname (* (array char))))

(define-alien-routine "if_nameindex" (* (array (struct if-nameindex) 0)))

(define-alien-routine "if_freenameindex" void
  (pointer (* (array (struct if-nameindex) 0))))


;;;;;;;;;;;;;;;;;;;
;;;             ;;;
;;; arpa/inet.h ;;;
;;;             ;;;
;;;;;;;;;;;;;;;;;;;

(define-alien-routine "inet_ntop" c-string
  (family int)
  (src (* t))
  (dest (* (array char 0)))
  (size socklen-t))

(define-alien-routine "inet_pton" int
  (family int)
  (src c-string)
  (dest (* t)))


;;;;;;;;;;;;;;;;
;;;          ;;;
;;; string.h ;;;
;;;          ;;;
;;;;;;;;;;;;;;;;

(define-alien-routine "memset" (* t)
  (buffer (* t))
  (value int)
  (length size-t))
