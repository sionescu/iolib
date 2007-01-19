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

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

;; TODO: manage socket options errors
(defun sockopt-error (retval level option action &optional val1 val2)
  (declare (type symbol action))
  (error "Sockopt error !"))

;;
;; Set option helpers
;;

(defun set-socket-option-bool (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-ref optval :int) (lisp->c-bool value))
    (et:setsockopt fd level option optval et:size-of-int)
    (values)))

(defun set-socket-option-int (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-ref optval :int) value)
    (et:setsockopt fd level option optval et:size-of-int)
    (values)))

(defun set-socket-option-linger (fd level option onoff linger)
  (with-foreign-object (optval 'et:linger)
    (setf (foreign-slot-value optval 'et:linger 'et:onoff) (lisp->c-bool onoff))
    (setf (foreign-slot-value optval 'et:linger 'et:linger) linger)
    (et:setsockopt fd level option optval et:size-of-linger)
    (values)))

(defun set-socket-option-timeval (fd level option sec usec)
  (with-foreign-object (optval 'et:timeval)
    (setf (foreign-slot-value optval 'et:timeval 'et:tv-sec) sec)
    (setf (foreign-slot-value optval 'et:timeval 'et:tv-usec) usec)
    (et:setsockopt fd level option optval et:size-of-timeval)
    (values)))

;;
;; Get option helpers
;;

(defun get-socket-option-bool (fd level option)
  (with-foreign-objects ((optval :int)
                         (optlen :socklen))
    (setf (mem-ref optlen :socklen) et:size-of-int)
    (et:getsockopt fd level option optval optlen)
    (values (c->lisp-bool (mem-ref optval :int)))))

(defun get-socket-option-int (fd level option)
  (with-foreign-objects ((optval :int)
                         (optlen :socklen))
    (setf (mem-ref optlen :socklen) et:size-of-int)
    (et:getsockopt fd level option optval optlen)
    (values (mem-ref optval :int))))

(defun get-socket-option-linger (fd level option)
  (with-foreign-objects ((optval 'et:linger)
                         (optlen :socklen))
    (setf (mem-ref optlen :socklen) et:size-of-linger)
    (et:getsockopt fd level option optval optlen)
    (values (c->lisp-bool (foreign-slot-value optval 'et:linger 'et:onoff))
            (foreign-slot-value optval 'et:linger 'et:linger))))

(defun get-socket-option-timeval (fd level option)
  (with-foreign-objects ((optval 'et:timeval)
                         (optlen :socklen))
    (setf (mem-ref optlen :socklen) et:size-of-timeval)
    (et:getsockopt fd level option optval optlen)
    (values (foreign-slot-value optval 'et:timeval 'et:tv-sec)
            (foreign-slot-value optval 'et:timeval 'et:tv-usec))))


;;
;; Option definitions
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-sockopt-helper-name (action value-type)
    (iolib-utils:concat-symbol action
                               '-socket-option-
                               value-type))

  (defparameter +helper-args-map+
    '((:bool (value))
      (:int (value))
      (:linger (onoff linger))
      (:timeval (sec usec)))))
  
(defmacro define-get-sockopt (os eql-name helper-get level optname)
  `(defmethod get-socket-option ((socket socket) (option-name (eql ,eql-name)))
     ,(if (member os *features*)
          `(with-socket-error-filter
             (,helper-get (socket-fd socket) ,level ,optname))
          `(error 'option-not-available option-name))))

(defmacro define-set-sockopt (os eql-name args helper-set level optname)
  `(defmethod set-socket-option ((socket socket) (option-name (eql ,eql-name)) &key ,@args)
     ,@(if (member os *features*)
           `((with-socket-error-filter
               (,helper-set (socket-fd socket) ,level ,optname ,@args)))
           `((declare (ignore ,@args))
             (error 'option-not-available option-name)))))

(defmacro define-socket-option (name action optname level argtype os)
  (declare (type symbol action)
           (type symbol argtype)
           (type symbol os))
  (let ((eql-name (iolib-utils:ensure-keyword name))
        (args (second (assoc argtype +helper-args-map+)))
        (helper-get (make-sockopt-helper-name :get argtype))
        (helper-set (make-sockopt-helper-name :set argtype)))
    `(progn
       ,@(remove-if #'null
           (list
            (when (member action (list :get :get-and-set))
              `(define-get-sockopt ,os ,eql-name ,helper-get ,level ,optname))
            (when (member action (list :set :get-and-set))
              `(define-set-sockopt ,os ,eql-name ,args ,helper-set ,level ,optname)))))))

;;;;;;;;;;;;;;;;;;;;;
;; Generic options ;;
;;;;;;;;;;;;;;;;;;;;;
(define-socket-option accept-connections :get         et::so-acceptconn   et::sol-socket :bool    :unix)
(define-socket-option broadcast          :get-and-set et::so-broadcast    et::sol-socket :bool    :unix)
(define-socket-option debug              :get-and-set et::so-debug        et::sol-socket :bool    :unix)
(define-socket-option dont-route         :get-and-set et::so-dontroute    et::sol-socket :bool    :unix)
(define-socket-option error              :get         et::so-error        et::sol-socket :int     :unix)
(define-socket-option keep-alive         :get-and-set et::so-keepalive    et::sol-socket :bool    :unix)
(define-socket-option linger             :get-and-set et::so-linger       et::sol-socket :linger  :unix)
(define-socket-option oob-inline         :get-and-set et::so-oobinline    et::sol-socket :bool    :unix)
(define-socket-option receive-buffer     :get-and-set et::so-rcvbuf       et::sol-socket :int     :unix)
(define-socket-option send-buffer        :get-and-set et::so-sndbuf       et::sol-socket :int     :unix)
(define-socket-option receive-low-water  :get-and-set et::so-rcvlowat     et::sol-socket :int     :unix)
(define-socket-option send-low-water     :get-and-set et::so-sndlowat     et::sol-socket :int     :unix)
(define-socket-option receive-timeout    :get-and-set et::so-rcvtimeo     et::sol-socket :timeval :unix)
(define-socket-option send-timeout       :get-and-set et::so-sndtimeo     et::sol-socket :timeval :unix)
(define-socket-option reuse-address      :get-and-set et::so-reuseaddr    et::sol-socket :bool    :unix)
(define-socket-option type               :get         et::so-type         et::sol-socket :int     :unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-socket-option bsd-compatible     :set         et::so-bsdcompat    et::sol-socket :bool    :linux)
(define-socket-option bind-to-device     :set         et::so-bindtodevice et::sol-socket :int     :linux)
(define-socket-option priority           :get-and-set et::so-priority     et::sol-socket :int     :linux)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FreeBSD-specific options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-socket-option reuse-port         :get-and-set et::so-reuseport    et::sol-socket :bool    :freebsd)
(define-socket-option use-loopback       :get-and-set et::so-useloopback  et::sol-socket :bool    :freebsd)
(define-socket-option no-sigpipe         :get-and-set et::so-nosigpipe    et::sol-socket :bool    :freebsd)


;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;; Still to be done ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: implement "struct ucred" helpers

;; (define-socket-option pass-credentials   :get-and-set et::so-passcred     et::sol-socket :ucred   :freebsd)
;; (define-socket-option peer-credentials   :get         et::so-peercred     et::sol-socket :ucred   :freebsd)


;; TODO: implement "struct accept_filter_arg" helpers

;; (define-socket-option accept-filter      :get-and-set et::so-acceptfilter et::sol-socket :accept-filter :freebsd)

;; TODO: find out the types of these options

;; (define-socket-option bintime            :get-and-set et::so-bintime      et::sol-socket :bool    :freebsd)
;; (define-socket-option label              :get-and-set et::so-label        et::sol-socket :bool    :freebsd)
;; (define-socket-option peerlabel          :get-and-set et::so-peerlabel    et::sol-socket :bool    :freebsd)
;; (define-socket-option listen-queue-limit :get-and-set et::so-listenqlimit et::sol-socket :int     :freebsd)
;; (define-socket-option listen-queue-length :get-and-set et::so-listenqlen  et::sol-socket :int     :freebsd)
;; (define-socket-option listen-incomplete-queue-length :get-and-set et::so-listenincqlen  et::sol-socket :int :freebsd)
