;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Grovel definitions for IO.MULTIPLEX
;;;
;;; Copyright (C) 2005-2006, Matthew Backes  <lucca@accela.net>
;;; Copyright (C) 2005-2006, Dan Knapp  <dankna@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu-@common-lisp.net>
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

#+linux
(define "_GNU_SOURCE")

;;; largefile support on linux
;;; TODO: check if these flags are required on solaris too
#+linux
(progn
  (define "_LARGEFILE_SOURCE")
  (define "_LARGEFILE64_SOURCE")
  (define "_FILE_OFFSET_BITS" 64))

(include "sys/types.h" "time.h" "sys/select.h" "sys/poll.h")

#+linux
(include "sys/epoll.h" "sys/ioctl.h")

#+bsd
(include "sys/event.h" "sys/time.h")    ; for kqueue

(in-package :io.multiplex)


;;;; from time.h

(ctype time_t "time_t")
(ctype suseconds "suseconds_t")

#-darwin
(progn
  (ctype clockid "clockid_t")
  (constant (clock-monotonic "CLOCK_MONOTONIC"))
  (constant (clock-realtime "CLOCK_REALTIME")))

(cstruct timespec "struct timespec"
  "UNIX time specification in seconds and nanoseconds."
  (sec  "tv_sec"  :type time_t)
  (nsec "tv_nsec" :type :long))

;;;; from sys/select.h

(constant (fd-setsize "FD_SETSIZE"))

(cstruct fd-set "fd_set"
  (bits "fds_bits" :type :uint8 :count :auto))

;;;; from sys/poll.h

(ctype nfds "nfds_t")

(cstruct pollfd "struct pollfd"
  "Poll file descriptor activity specification structure."
  (fd      "fd"      :type :int)
  (events  "events"  :type :short)
  (revents "revents" :type :short))

(constant (pollin "POLLIN"))
(constant (pollrdnorm "POLLRDNORM"))
(constant (pollrdband "POLLRDBAND"))
(constant (pollpri "POLLPRI"))
(constant (pollout "POLLOUT"))
(constant (pollwrnorm "POLLWRNORM"))
(constant (pollwrband "POLLWRBAND"))
(constant (pollerr "POLLERR"))
#-darwin (constant (pollrdhup "POLLRDHUP"))
(constant (pollhup "POLLHUP"))
(constant (pollnval "POLLNVAL"))

;;;; from sys/epoll.h

#+linux
(progn
  (cunion epoll-data "epoll_data_t"
    (ptr "ptr" :type :pointer)
    (fd  "fd"  :type :int)
    (u32 "u32" :type :uint32)
    (u64 "u64" :type :uint64))

  (cstruct epoll-event "struct epoll_event"
    (events "events" :type :uint32)
    (data   "data"   :type epoll-data))

  (constant (epollin "EPOLLIN"))
  (constant (epollrdnorm "EPOLLRDNORM"))
  (constant (epollrdband "EPOLLRDBAND"))
  (constant (epollpri "EPOLLPRI"))
  (constant (epollout "EPOLLOUT"))
  (constant (epollwrnorm "EPOLLWRNORM"))
  (constant (epollwrband "EPOLLWRBAND"))
  (constant (epollerr "EPOLLERR"))
  (constant (epollhup "EPOLLHUP"))
  (constant (epollmsg "EPOLLMSG"))
  (constant (epolloneshot "EPOLLONESHOT"))
  (constant (epollet "EPOLLET"))

  (constant (epoll-ctl-add "EPOLL_CTL_ADD"))
  (constant (epoll-ctl-del "EPOLL_CTL_DEL"))
  (constant (epoll-ctl-mod "EPOLL_CTL_MOD")))

;;;; from sys/event.h

#+bsd
(progn
  (ctype intptr "intptr_t")
  (ctype uintptr "uintptr_t")

  (cstruct kevent "struct kevent"
    (ident  "ident"  :type uintptr)
    (filter "filter" :type :short)
    (flags  "flags"  :type :unsigned-short)
    (fflags "fflags" :type :unsigned-int)
    (data   "data"   :type intptr)
    (udata  "udata"  :type :pointer))

  ;; kevent() flags
  (constant (ev-add "EV_ADD"))
  (constant (ev-enable "EV_ENABLE"))
  (constant (ev-disable "EV_DISABLE"))
  (constant (ev-delete "EV_DELETE"))
  (constant (ev-oneshot "EV_ONESHOT"))
  (constant (ev-clear "EV_CLEAR"))
  (constant (ev-eof "EV_EOF"))
  (constant (ev-error "EV_ERROR"))

  ;; kevent() filter flags
  (constant (evfilt-read "EVFILT_READ"))
  (constant (evfilt-write "EVFILT_WRITE"))
  (constant (evfilt-aio "EVFILT_AIO"))
  (constant (evfilt-vnode "EVFILT_VNODE"))
  (constant (evfilt-proc "EVFILT_PROC"))
  (constant (evfilt-signal "EVFILT_SIGNAL"))
  (constant (evfilt-timer "EVFILT_TIMER"))
  #-darwin (constant (evfilt-netdev "EVFILT_NETDEV"))

  ;; EVFILT_VNODE options
  (constant (note-delete "NOTE_DELETE"))
  (constant (note-write "NOTE_WRITE"))
  (constant (note-extend "NOTE_EXTEND"))
  (constant (note-attrib "NOTE_ATTRIB"))
  (constant (note-link "NOTE_LINK"))
  (constant (note-rename "NOTE_RENAME"))
  (constant (note-revoke "NOTE_REVOKE"))

  ;; EVFILT_PROC options
  (constant (note-exit "NOTE_EXIT"))
  (constant (note-fork "NOTE_FORK"))
  (constant (note-exec "NOTE_EXEC"))
  (constant (note-track "NOTE_TRACK"))
  (constant (note-trackerr "NOTE_TRACKERR"))

  ;; EVFILT_NETDEV options
  #-darwin
  (progn
    (constant (note-linkup "NOTE_LINKUP"))
    (constant (note-linkdown "NOTE_LINKDOWN"))
    (constant (note-linkinv "NOTE_LINKINV"))))
