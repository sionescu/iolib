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

(in-package #:net.sockets)

(defun manage-sockopt-error (retval level option action &optional val1 val2)
  )

;;
;; Set Options
;;

(defun set-socket-option-bool (fd level option value)
  (with-alien ((optval int))
    (sb-sys:with-pinned-objects (optval)
      (setf optval (lisp->c-bool value))
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-int)))
        (if (zerop retval)
            (return-from set-socket-option-bool
              (values))
            (manage-sockopt-error retval level option :set value))))))

(defun set-socket-option-int (fd level option value)
  (with-alien ((optval int))
    (sb-sys:with-pinned-objects (optval)
      (setf optval value)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-int)))
        (if (zerop retval)
            (return-from set-socket-option-int
              (values))
            (manage-sockopt-error retval level option :set value))))))

(defun set-socket-option-linger (fd level option onoff linger)
  (with-alien ((optval (struct et::linger)))
    (sb-sys:with-pinned-objects (optval)
      (setf (slot optval 'et::onoff) onoff)
      (setf (slot optval 'et::linger) linger)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-linger)))
        (if (zerop retval)
            (return-from set-socket-option-linger
              (values))
            (manage-sockopt-error retval level option :set onoff linger))))))

(defun set-socket-option-timeval (fd level option sec usec)
  (with-alien ((optval (struct et::timeval)))
    (sb-sys:with-pinned-objects (optval)
      (setf (slot optval 'et::tv-sec) sec)
      (setf (slot optval 'et::tv-usec) usec)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-timeval)))
        (if (zerop retval)
            (return-from set-socket-option-timeval
              (values))
            (manage-sockopt-error retval level option :set sec usec))))))

;;
;; Get Options
;;

(defun get-socket-option-bool (fd level option)
  (with-alien ((optval int))
    (sb-sys:with-pinned-objects (optval)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-int)))
        (if (zerop retval)
            (return-from get-socket-option-bool
              (values (c->lisp-bool optval)))
            (manage-sockopt-error retval level option :get))))))

(defun get-socket-option-int (fd level option)
  (with-alien ((optval int))
    (sb-sys:with-pinned-objects (optval)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-int)))
        (if (zerop retval)
            (return-from get-socket-option-int
              (values optval))
            (manage-sockopt-error retval level option :get))))))

(defun get-socket-option-linger (fd level option)
  (with-alien ((optval (struct et::linger)))
    (sb-sys:with-pinned-objects (optval)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-linger)))
        (if (zerop retval)
            (return-from get-socket-option-linger
              (values (slot optval 'et::onoff)
                      (slot optval 'et::linger)))
            (manage-sockopt-error retval level option :get))))))

(defun get-socket-option-timeval (fd level option)
  (with-alien ((optval (struct et::timeval)))
    (sb-sys:with-pinned-objects (optval)
      (let ((retval
             (et::setsockopt fd level option (addr optval) et::size-of-timeval)))
        (if (zerop retval)
            (return-from get-socket-option-timeval
              (values (slot optval 'et::tv-sec)
                      (slot optval 'et::tv-usec)))
            (manage-sockopt-error retval level option :get))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-sockopt-name (base-name)
    (read-from-string (concatenate 'string
                                   "sockopt-"
                                   (symbol-name base-name))))

  (defun make-sockopt-helper-name (action value-type)
    (read-from-string (concatenate 'string
                                   (symbol-name action)
                                   "-socket-option-"
                                   (symbol-name value-type))))

  (defparameter +argument-types-map+
    '((:bool (value))
      (:int (value))
      (:linger ((car value) (cdr value)))
      (:timeval ((car value) (cdr value))))))

(defmacro define-socket-option (name action optname level argtype)
  (let ((funcname (make-sockopt-name name))
        (helper-get (make-sockopt-helper-name :get argtype))
        (helper-set (make-sockopt-helper-name :set argtype))
        (args (second (assoc argtype +argument-types-map+))))
    `(progn
       ,@(remove-if
          #'null
          (list
           (if (member action (list :get :get-and-set))
               `(defmethod ,funcname ((socket socket))
                  (,helper-get (socket-fd socket) ,level ,optname))
               (values))
           (if (member action (list :set :get-and-set))
               `(defmethod (setf ,funcname) (value (socket socket))
                  (,helper-set (socket-fd socket) ,level ,optname ,@args))
               (values)))))))

(define-socket-option accept-connections :get         et::so-acceptconn   et::sol-socket :bool)
(define-socket-option broadcast          :get-and-set et::so-broadcast    et::sol-socket :bool)
(define-socket-option debug              :get-and-set et::so-debug        et::sol-socket :bool)
(define-socket-option dont-route         :get-and-set et::so-dontroute    et::sol-socket :bool)
(define-socket-option error              :get         et::so-error        et::sol-socket :int)
(define-socket-option keep-alive         :get-and-set et::so-keepalive    et::sol-socket :bool)
(define-socket-option linger             :get-and-set et::so-linger       et::sol-socket :linger)
(define-socket-option oob-inline         :get-and-set et::so-oobinline    et::sol-socket :bool)
(define-socket-option receive-buffer     :get-and-set et::so-rcvbuf       et::sol-socket :int)
(define-socket-option send-buffer        :get-and-set et::so-sndbuf       et::sol-socket :int)
(define-socket-option receive-low-water  :get-and-set et::so-rcvlowat     et::sol-socket :int)
(define-socket-option send-low-water     :get-and-set et::so-sndlowat     et::sol-socket :int)
(define-socket-option receive-timeout    :get-and-set et::so-rcvtimeo     et::sol-socket :timeval)
(define-socket-option send-timeout       :get-and-set et::so-sndtimeo     et::sol-socket :timeval)
(define-socket-option reuse-address      :get-and-set et::so-reuseaddr    et::sol-socket :bool)
(define-socket-option type               :get         et::so-type         et::sol-socket :int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(define-socket-option bsd-compatible     :set         et::so-bsdcompat    et::sol-socket :bool)
#+linux
(define-socket-option bind-to-device     :set         et::so-bindtodevice et::sol-socket :int)
#+linux
(define-socket-option priority           :get-and-set et::so-priority     et::sol-socket :int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FreeBSD-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+freebsd
(define-socket-option reuse-port         :get-and-set et::so-reuseport    et::sol-socket :bool)
#+freebsd
(define-socket-option use-loopback       :get-and-set et::so-useloopback  et::sol-socket :bool)
#+freebsd
(define-socket-option no-sigpipe         :get-and-set et::so-nosigpipe    et::sol-socket :bool)


#| Still to be done

;; TODO: implement "struct ucred" helpers

#+linux
(define-socket-option pass-credentials   :get-and-set et::so-passcred     et::sol-socket :ucred)
#+linux
(define-socket-option peer-credentials   :get         et::so-peercred     et::sol-socket :ucred)


;; TODO: implement "struct accept_filter_arg" helpers

#+freebsd
(define-socket-option accept-filter      :get-and-set et::so-acceptfilter et::sol-socket :accept-filter)

;; TODO: find out the types of these options

#+freebsd
(define-socket-option bintime            :get-and-set et::so-bintime      et::sol-socket :bool)
#+freebsd
(define-socket-option label              :get-and-set et::so-label        et::sol-socket :bool)
#+freebsd
(define-socket-option peerlabel          :get-and-set et::so-peerlabel    et::sol-socket :bool)
#+freebsd
(define-socket-option listen-queue-limit :get-and-set et::so-listenqlimit  et::sol-socket :int)
#+freebsd
(define-socket-option listen-queue-length :get-and-set  et::so-listenqlen  et::sol-socket :int)
#+freebsd
(define-socket-option listen-incomplete-queue-length :get-and-set et::so-listenincqlen  et::sol-socket :int)

|#
