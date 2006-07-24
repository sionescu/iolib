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

(in-package #:net.sockets)

;; TODO: manage socket errors
(defun manage-socket-error (err)
  (error err))

(defmacro define-wrapper-function (funcname)
  (let* ((alien-name (intern (string-upcase (symbol-name funcname))
                             'et))
         (arglist (mapcar #'(lambda (sym)
                              (intern (string-upcase (symbol-name sym))))
                          (sb-introspect:function-arglist alien-name))))
    `(defun ,funcname ,arglist
       (handler-case
           (,alien-name ,@arglist)
         (et:unix-error (err)
           (manage-socket-error err))))))

(defmacro define-all-wrappers (funclist)
  `(progn
     ,@(loop :for func :in funclist
          :collect `(define-wrapper-function ,func))))

(define-all-wrappers
    (accept bind close connect getpeername getsockname
            getsockopt listen recv recvfrom recvmsg send
            sendmsg sendto setsockopt shutdown socket
            sockatmark socketpair))
