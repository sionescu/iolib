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

(in-package #:io.multiplex)

;;; TODO: do real detecting here
(setf *multiplex-best-interface*
      'select-multiplex-interface)

(defmethod set-fd-handlers progn ((interface multiplex-interface) fd
                                  &key read-handler write-handler except-handler)
  (assert (or read-handler write-handler except-handler))

  (with-slots (fd-handlers) interface
    (let ((current-handler (gethash fd fd-handlers)))
      (if current-handler
          (progn
            (when read-handler
              (setf (handler-read-func current-handler) read-handler))
            (when write-handler
              (setf (handler-write-func current-handler) write-handler))
            (when except-handler
              (setf (handler-except-func current-handler) except-handler)))
          ;; there is no handler installed so make a new one
          (setf current-handler (make-handler fd read-handler write-handler except-handler)))
      (setf (gethash fd fd-handlers) current-handler))))

(defmethod remove-fd-handlers progn ((interface multiplex-interface) fd
                                     &key read write except all)
  (unless all
    (assert (or read write except)))

  (with-slots (fd-handlers) interface
    (let ((current-handler (gethash fd fd-handlers)))
      (when current-handler
        (if all
            (remhash fd fd-handlers)
            (progn
              (when read   (setf (handler-read-func   current-handler) nil))
              (when write  (setf (handler-write-func  current-handler) nil))
              (when except (setf (handler-except-func current-handler) nil))
              (if (or (handler-read-func   current-handler)
                      (handler-write-func  current-handler)
                      (handler-except-func current-handler))
                  (setf (gethash fd fd-handlers) current-handler)
                  (remhash fd fd-handlers))))))))

;; if there are handlers installed save them and restore them at the end
;; (defmacro with-fd-handlers ((fd &key read-handler write-handler except-handler) &body body)
;;   (let ((tmp-handler (gensym)))
;;     `(let ((,tmp-handler (gethash ,fd fd-handlers)))
;;        (unwind-protect
;;             (progn
;;               (when ,tmp-handler
;;                 (remove-fd-handlers ,fd :all t))
;;               (set-fd-handlers ,fd :read-handler ,read-handler
;;                                    :write-handler ,write-handler
;;                                    :except-handler ,except-handler)
;;               ,@body)
;;          (if ,tmp-handler
;;              (setf (gethash ,fd fd-handlers) ,tmp-handler)
;;              (remove-fd-handlers ,fd :all t))))))
