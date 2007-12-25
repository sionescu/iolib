;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; package.lisp --- Package definition.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :common-lisp-user)

(defpackage :io.multiplex
  (:nicknames #:iomux)
  (:use #:common-lisp :cffi :alexandria :series)
  (:export
   ;; Classes
   #:event
   #:event-base
   #:multiplexer
   #:select-multiplexer
   #:poll-multiplexer
   #+bsd #:kqueue-multiplexer
   #+linux #:epoll-multiplexer
   #:fd-event
   #:priority-queue

   ;; Event-base Operations
   #:*available-multiplexers*
   #:*default-multiplexer*
   #:*default-event-loop-timeout*
   #:add-fd
   #:add-timer
   #:event-base-empty-p
   #:event-dispatch
   #:exit-event-loop
   #:remove-event
   #:remove-fd
   #:with-event-base

   ;; Operations on FDs
   #:fd-readablep
   #:fd-ready-p
   #:fd-writablep
   #:poll-error
   #:poll-error-fd
   #:poll-error-identifier
   #:wait-until-fd-ready
   ))

(series::install)
