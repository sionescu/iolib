;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; socket-options.lisp --- Setter and getters for various socket options.
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

(in-package :net.sockets)

;;; TODO: manage socket options errors
(defun sockopt-error (retval level option action &optional val1 val2)
  (declare (ignore retval level option action val1 val2))
  (error "Sockopt error !"))

;;;; SETF

;;; This interface looks nice but doesn't work so well for the linger
;;; and timeout options.  Figure out a good solution.  Possible ones
;;; include:
;;;
;;;    * don't worry, tell the user to use GET/SET-SOCKET-OPTION
;;;    * socket-linger-option and socket-timeval-option accessors
;;;    * use separate accessors for each and every option, like
;;;      SB-BSD-SOCKETS.

(defun socket-option (socket option-name)
  (get-socket-option socket option-name))

(defun (setf socket-option) (value socket option-name)
  (set-socket-option socket option-name :value value))

;;;; Set Helpers

(defun set-socket-option-bool (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-ref optval :int) (lisp->c-bool value))
    (setsockopt fd level option optval size-of-int)
    (values)))

(defun set-socket-option-int (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-ref optval :int) value)
    (setsockopt fd level option optval size-of-int)
    (values)))

(defun set-socket-option-linger (fd level option new-onoff new-linger)
  (with-foreign-object (optval 'linger)
    (with-foreign-slots ((linger onoff) optval linger)
      (setf onoff (lisp->c-bool new-onoff)
            linger new-linger))
    (setsockopt fd level option optval size-of-linger)
    (values)))

#-windows
(defun set-socket-option-timeval (fd level option sec usec)
  (with-foreign-object (optval 'nix::timeval)
    (with-foreign-slots ((nix::sec nix::usec) optval nix::timeval)
      (setf nix::sec sec
            nix::usec usec))
    (setsockopt fd level option optval nix::size-of-timeval)
    (values)))

;;;; Get Helpers

(defun get-socket-option-bool (fd level option)
  (with-foreign-object (optval :int)
    (with-socklen (optlen size-of-int)
      (getsockopt fd level option optval optlen)
      (mem-ref optval :boolean))))

(defun get-socket-option-int (fd level option)
  (with-foreign-object (optval :int)
    (with-socklen (optlen size-of-int)
      (getsockopt fd level option optval optlen)
      (mem-ref optval :int))))

(defun get-socket-option-linger (fd level option)
  (with-foreign-object (optval 'linger)
    (with-socklen (optlen size-of-linger)
      (getsockopt fd level option optval optlen)
      (with-foreign-slots ((linger onoff) optval linger)
        (values (not (zerop onoff)) linger)))))

#-windows
(defun get-socket-option-timeval (fd level option)
  (with-foreign-object (optval 'nix::timeval)
    (with-socklen (optlen nix::size-of-timeval)
      (getsockopt fd level option optval optlen)
      (with-foreign-slots ((nix::sec nix::usec) optval nix::timeval)
        (values nix::sec nix::usec)))))

;;;; Option Definitions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +helper-args-map+
    '((:bool (value))
      (:int (value))
      (:linger (onoff linger))
      (:timeval (sec usec)))))

(defmacro define-get-sockopt (os eql-name helper-get level optname)
  `(defmethod get-socket-option ((socket socket) (option-name (eql ,eql-name)))
     ,(if (alexandria:featurep os)
          `(,helper-get (socket-fd socket) ,level ,optname)
          `(error 'option-not-available option-name))))

(defmacro define-set-sockopt (os eql-name args helper-set level optname)
  `(defmethod set-socket-option
       ((socket socket) (option-name (eql ,eql-name)) &key ,@args)
     ,@(if (alexandria:featurep os)
           `((,helper-set (socket-fd socket) ,level ,optname ,@args))
           `((declare (ignore ,@args))
             (error 'option-not-available option-name)))))

(defmacro define-socket-option (name action optname level argtype os)
  (declare (type symbol action)
           (type symbol argtype)
           (type (or symbol list) os))
  (flet ((make-helper-name (action value-type)
           (alexandria:format-symbol t "~A~A~A"
                                     action '#:-socket-option- value-type)))
    (let ((eql-name (alexandria:make-keyword name))
          (args (second (assoc argtype +helper-args-map+)))
          (helper-get (make-helper-name :get argtype))
          (helper-set (make-helper-name :set argtype)))
      `(progn
         ,@(remove-if
            #'null
            (list
             (when (member action (list :get :get-and-set))
               `(define-get-sockopt ,os ,eql-name ,helper-get ,level ,optname))
             (when (member action (list :set :get-and-set))
               `(define-set-sockopt ,os ,eql-name ,args ,helper-set ,level
                                    ,optname))))))))

(defmacro define-socket-options (action level os &body options)
  `(progn
     ,@(loop :for (name optname argtype) :in options :collect
             `(define-socket-option ,name ,action
                ,optname ,level ,argtype ,os))))

;;;; Generic options

(define-socket-options :get sol-socket :unix
  (accept-connections so-acceptconn :bool)
  (error              so-error      :int)
  (type               so-type       :int))

(define-socket-options :get-and-set sol-socket :unix
  (broadcast         so-broadcast :bool)
  (debug             so-debug     :bool)
  (dont-route        so-dontroute :bool)
  (keep-alive        so-keepalive :bool)
  (linger            so-linger    :linger)
  (oob-inline        so-oobinline :bool)
  (receive-buffer    so-rcvbuf    :int)
  (send-buffer       so-sndbuf    :int)
  (receive-low-water so-rcvlowat  :int)
  (send-low-water    so-sndlowat  :int)
  #-windows (receive-timeout   so-rcvtimeo  :timeval)
  #-windows (send-timeout      so-sndtimeo  :timeval)
  (reuse-address     so-reuseaddr :bool))

;;;; Linux-specific Options

(define-socket-options :set sol-socket :linux
  (bsd-compatible so-bsdcompat    :bool)
  (bind-to-device so-bindtodevice :int))

(define-socket-option priority :get-and-set
  so-priority sol-socket :int :linux)

;;;; FreeBSD-specific options

(define-socket-options :get-and-set sol-socket :freebsd
  (reuse-port   so-reuseport   :bool)
  (use-loopback so-useloopback :bool)
  (no-sigpipe   so-nosigpipe   :bool))

;;;; TODO

;; TODO: implement "struct ucred" helpers

;; (define-socket-option pass-credentials   :get-and-set et:so-passcred     et:sol-socket :ucred   (:or :linux :freebsd))
;; (define-socket-option peer-credentials   :get         et:so-peercred     et:sol-socket :ucred   (:or :linux :freebsd))


;; TODO: implement "struct accept_filter_arg" helpers

;; (define-socket-option accept-filter      :get-and-set et:so-acceptfilter et:sol-socket :accept-filter :freebsd)

;; TODO: find out the types of these options

;; (define-socket-option bintime            :get-and-set et:so-bintime      et:sol-socket :bool    :freebsd)
;; (define-socket-option label              :get-and-set et:so-label        et:sol-socket :bool    :freebsd)
;; (define-socket-option peerlabel          :get-and-set et:so-peerlabel    et:sol-socket :bool    :freebsd)
;; (define-socket-option listen-queue-limit :get-and-set et:so-listenqlimit et:sol-socket :int     :freebsd)
;; (define-socket-option listen-queue-length :get-and-set et:so-listenqlen  et:sol-socket :int     :freebsd)
;; (define-socket-option listen-incomplete-queue-length :get-and-set et:so-listenincqlen  et:sol-socket :int :freebsd)


;;;; TCP Options

(define-socket-option tcp-nodelay :get-and-set
  tcp-nodelay ipproto-tcp :bool :unix)

(define-socket-option tcp-maxseg :get-and-set
  tcp-maxseg ipproto-tcp :int (:or :linux :freebsd))

;;;; Linux-specific TCP Options

(define-socket-options :get-and-set ipproto-tcp :linux
  (tcp-cork         tcp-cork         :bool)
  (tcp-defer-accept tcp-defer-accept :int)
  (tcp-keepcnt      tcp-keepcnt      :int)
  (tcp-keepidle     tcp-keepidle     :int)
  (tcp-keepintvl    tcp-keepintvl    :int)
  (tcp-linger2      tcp-linger2      :int)
  (tcp-quickack     tcp-quickack     :bool)
  (tcp-syncnt       tcp-syncnt       :int)
  (tcp-window-clamp tcp-window-clamp :int))

;; TODO: implement "struct tcp_info" helper
;; (define-socket-option tcp-info         :get         et::tcp-info         et:ipproto-tcp :tcp-info :linux)

;;;; FreeBSD-specific TCP Options

(define-socket-options :get-and-set ipproto-tcp :freebsd
  (tcp-noopt  tcp-noopt  :bool)
  (tcp-nopush tcp-nopush :bool))
