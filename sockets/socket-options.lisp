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

;; (declaim (optimize (speed 0) (safety 3) (space 0) (debug 2)))
(declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))

(in-package #:net.sockets)

;; TODO: manage socket options errors
(defun manage-sockopt-error (retval level option action &optional val1 val2)
  (declare (type symbol action)))

;;
;; Set Options
;;

(defun set-socket-option-bool (fd level option value)
  (with-pinned-aliens ((optval int))
    (setf optval (lisp->c-bool value))
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-int)
    (values)))

(defun set-socket-option-int (fd level option value)
  (with-pinned-aliens ((optval int))
    (setf optval value)
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-int)
    (values)))

(defun set-socket-option-linger (fd level option onoff linger)
  (with-pinned-aliens ((optval sb-posix::linger))
    (setf (slot optval 'sb-posix::onoff) onoff)
    (setf (slot optval 'sb-posix::linger) linger)
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-linger)
    (values)))

(defun set-socket-option-timeval (fd level option sec usec)
  (with-pinned-aliens ((optval sb-posix::timeval))
    (setf (slot optval 'sb-posix::tv-sec) sec)
    (setf (slot optval 'sb-posix::tv-usec) usec)
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-timeval)
    (values)))

;;
;; Get Options
;;

(defun get-socket-option-bool (fd level option)
  (with-pinned-aliens ((optval int))
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-int)
    (c->lisp-bool optval)))

(defun get-socket-option-int (fd level option)
  (with-pinned-aliens ((optval int))
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-int)
    optval))

(defun get-socket-option-linger (fd level option)
  (with-pinned-aliens ((optval sb-posix::linger))
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-linger)
    (values (slot optval 'sb-posix::onoff)
            (slot optval 'sb-posix::linger))))

(defun get-socket-option-timeval (fd level option)
  (with-pinned-aliens ((optval sb-posix::timeval))
    (sb-posix::setsockopt fd level option (addr optval) sb-posix::size-of-timeval)
    (values (slot optval 'sb-posix::tv-sec)
            (slot optval 'sb-posix::tv-usec))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (sym)
    (read-from-string
     (concatenate 'string ":" (symbol-name sym))))

  (defun make-sockopt-helper-name (action value-type)
    (read-from-string (concatenate 'string
                                   (symbol-name action)
                                   "-socket-option-"
                                   (symbol-name value-type))))

  (defparameter +helper-args-map+
    '((:bool (value))
      (:int (value))
      (:linger (onoff linger))
      (:timeval (sec usec)))))

(defgeneric get-socket-option (socket option-name))

(defgeneric set-socket-option (socket option-name &key &allow-other-keys))

(defmacro define-socket-option (name action optname level argtype os)
  (declare (type symbol action)
           (type symbol argtype)
           (type symbol os))
  (let ((eql-name (make-keyword name))
        (args (second (assoc argtype +helper-args-map+)))
        (helper-get (make-sockopt-helper-name :get argtype))
        (helper-set (make-sockopt-helper-name :set argtype)))
    `(progn
       ,@(remove-if
          #'null
          (list
           (when (member action (list :get :get-and-set))
             `(defmethod get-socket-option ((socket socket) (option-name (eql ,eql-name)))
                ,(if (member os *features*)
                     `(handler-case
                          (,helper-get (socket-fd socket) ,level ,optname)
                        (sb-posix:syscall-error (err)
                          (manage-sockopt-error (sb-posix:syscall-errno err)
                                                ,level ,optname :get)))
                     `(error 'option-not-available option-name))))
           (when (member action (list :set :get-and-set))
             `(defmethod set-socket-option ((socket socket) (option-name (eql ,eql-name)) &key ,@args)
                ,(if (member os *features*)
                     `(handler-case
                          (,helper-set (socket-fd socket) ,level ,optname ,@args)
                        (sb-posix:syscall-error (err)
                          (manage-sockopt-error (sb-posix:syscall-errno err)
                                                ,level ,optname :set ,@args)))
                     `(error 'option-not-available option-name)))))))))

(define-socket-option accept-connections :get         sb-posix::so-acceptconn   sb-posix::sol-socket :bool    :unix)
(define-socket-option broadcast          :get-and-set sb-posix::so-broadcast    sb-posix::sol-socket :bool    :unix)
(define-socket-option debug              :get-and-set sb-posix::so-debug        sb-posix::sol-socket :bool    :unix)
(define-socket-option dont-route         :get-and-set sb-posix::so-dontroute    sb-posix::sol-socket :bool    :unix)
(define-socket-option error              :get         sb-posix::so-error        sb-posix::sol-socket :int     :unix)
(define-socket-option keep-alive         :get-and-set sb-posix::so-keepalive    sb-posix::sol-socket :bool    :unix)
(define-socket-option linger             :get-and-set sb-posix::so-linger       sb-posix::sol-socket :linger  :unix)
(define-socket-option oob-inline         :get-and-set sb-posix::so-oobinline    sb-posix::sol-socket :bool    :unix)
(define-socket-option receive-buffer     :get-and-set sb-posix::so-rcvbuf       sb-posix::sol-socket :int     :unix)
(define-socket-option send-buffer        :get-and-set sb-posix::so-sndbuf       sb-posix::sol-socket :int     :unix)
(define-socket-option receive-low-water  :get-and-set sb-posix::so-rcvlowat     sb-posix::sol-socket :int     :unix)
(define-socket-option send-low-water     :get-and-set sb-posix::so-sndlowat     sb-posix::sol-socket :int     :unix)
(define-socket-option receive-timeout    :get-and-set sb-posix::so-rcvtimeo     sb-posix::sol-socket :timeval :unix)
(define-socket-option send-timeout       :get-and-set sb-posix::so-sndtimeo     sb-posix::sol-socket :timeval :unix)
(define-socket-option reuse-address      :get-and-set sb-posix::so-reuseaddr    sb-posix::sol-socket :bool    :unix)
(define-socket-option type               :get         sb-posix::so-type         sb-posix::sol-socket :int     :unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-socket-option bsd-compatible     :set         sb-posix::so-bsdcompat    sb-posix::sol-socket :bool    :linux)
(define-socket-option bind-to-device     :set         sb-posix::so-bindtodevice sb-posix::sol-socket :int     :linux)
(define-socket-option priority           :get-and-set sb-posix::so-priority     sb-posix::sol-socket :int     :linux)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FreeBSD-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-socket-option reuse-port         :get-and-set sb-posix::so-reuseport    sb-posix::sol-socket :bool    :freebsd)
(define-socket-option use-loopback       :get-and-set sb-posix::so-useloopback  sb-posix::sol-socket :bool    :freebsd)
(define-socket-option no-sigpipe         :get-and-set sb-posix::so-nosigpipe    sb-posix::sol-socket :bool    :freebsd)


;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;; Still to be done ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: implement "struct ucred" helpers

;; (define-socket-option pass-credentials   :get-and-set sb-posix::so-passcred     sb-posix::sol-socket :ucred   :freebsd)
;; (define-socket-option peer-credentials   :get         sb-posix::so-peercred     sb-posix::sol-socket :ucred   :freebsd)


;; TODO: implement "struct accept_filter_arg" helpers

;; (define-socket-option accept-filter      :get-and-set sb-posix::so-acceptfilter sb-posix::sol-socket :accept-filter :freebsd)

;; TODO: find out the types of these options

;; (define-socket-option bintime            :get-and-set sb-posix::so-bintime      sb-posix::sol-socket :bool    :freebsd)
;; (define-socket-option label              :get-and-set sb-posix::so-label        sb-posix::sol-socket :bool    :freebsd)
;; (define-socket-option peerlabel          :get-and-set sb-posix::so-peerlabel    sb-posix::sol-socket :bool    :freebsd)
;; (define-socket-option listen-queue-limit :get-and-set sb-posix::so-listenqlimit sb-posix::sol-socket :int     :freebsd)
;; (define-socket-option listen-queue-length :get-and-set sb-posix::so-listenqlen  sb-posix::sol-socket :int     :freebsd)
;; (define-socket-option listen-incomplete-queue-length :get-and-set sb-posix::so-listenincqlen  sb-posix::sol-socket :int :freebsd)
