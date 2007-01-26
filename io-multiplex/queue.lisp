;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               queue.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    This module exports a queue type. This is a structure optimized for
;;;;    FIFO operations, keeping a pointer to the head and the tail of a list.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    <SI>  Stelian Ionescu <sionescu@common-lisp.net>
;;;;MODIFICATIONS
;;;;    2007-01-23 <SI>  Added QUEUE-SORTED-INSERT, QUEUE-FILTER-AND-DELETE,
;;;;                     QUEUE-DELETE-IF and QUEUE-FILTER
;;;;    2005-08-31 <PJB> Added QUEUE-DELETE
;;;;    2004-02-26 <PJB> Formated for publication.
;;;;    2001-12-31 <PJB> Added pjb-queue-requeue. 
;;;;                     Corrected the return value of some methods.
;;;;    2001-11-12 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2001 - 2005
;;;;              Stelian Ionescu 2007
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(in-package :io.multiplex)

;;; The structure of a queue is as follow:
;;; 
;;;                  queue
;;;                    |
;;;                    V
;;;             +------+------+
;;;             | head | tail |--------------------------+
;;;             +------+------+                          |
;;;                |                                     |
;;;                V                                     V
;;;         +------+------+    +------+------+    +------+------+
;;;         | car  | cdr  |--->| car  | cdr  |--->| car  | cdr  |--->nil
;;;         +------+------+    +------+------+    +------+------+
;;;            |                  |                  |
;;;            V                  V                  V
;;;         +------+           +------+           +------+
;;;         | elem |           | elem |           | elem |
;;;         +------+           +------+           +------+


(defstruct (queue (:constructor %make-queue))
  (head nil :type list)
  (tail nil :type list))


(defun queue-invariant (queue)
  (assert (queue-p queue))
  (assert (or (and (null (queue-head queue))  (null (queue-tail queue)))
              (and (queue-head queue) (queue-tail queue))))
  (when (queue-head queue)
    (assert (list-length (queue-head queue))) ; not a circular list.
    (assert (search (queue-tail queue) (queue-head queue) :test (function eq)))
    (assert (null (cdr (queue-tail queue))))))


(defun make-queue () (%make-queue))


(defun queue-length (queue)
  "
PRE:     (queue-p queue)
RETURN:  The number of elements in the queue.
"
  (assert (queue-p queue))
  (length (queue-head queue))) ;;queue-length


(defun queue-empty-p (queue)
  "
RETURN:  (= 0 (queue-length queue))
"
  (assert (queue-p queue))
  (null (queue-head queue)))


(defun queue-first-element (queue)
  "
PRE:     (queue-p queue)
RETURN:  The first element of the queue.
"
  (assert (queue-p queue))
  (first (queue-head queue)))


(defun queue-last-element (queue)
  "
PRE:     (queue-p queue)
RETURN:  The last element of the queue.
"
  (assert (queue-p queue))
  (first (queue-tail queue)))


(defun queue-enqueue  (queue element)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-last-element queue) element),
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (assert (queue-p queue))
  ;; (car q) = head      (cdr q) = tail
  (if (queue-head queue)
      (progn
        ;; There's already an element, just add to the tail.
        (setf (cdr (queue-tail queue)) (cons element nil))
        (setf (queue-tail queue)       (cdr (queue-tail queue))))
      (progn
        ;; The queue is empty, let's set the head.
        (setf (queue-head queue) (cons element nil))
        (setf (queue-tail queue) (queue-head queue))))
  queue)


(defun queue-delete (queue element &key (test (function eql)))
  "
POST:    (not (member element queue :test test))
RETURN:  queue
"
  (assert (queue-p queue))
  (setf (queue-head queue) (delete element (queue-head queue) :test test)
        (queue-tail queue) (last (queue-head queue)))
  queue)


(defun queue-delete-if (queue test)
  (assert (queue-p queue))
  (setf (queue-head queue) (delete-if test (queue-head queue))
        (queue-tail queue) (last (queue-head queue)))
  queue)


(defun queue-dequeue (queue)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
         f=(queue-first-element queue)
POST:    l>0 ==> l-1=(queue-length queue)
         l=0 ==> 0=(queue-length queue)
RETURN:  f
"
  (assert (queue-p queue))
  (prog1 (pop (queue-head queue))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))))


(defun queue-requeue (queue element)
  "
DO:      Insert the element at the beginning of the queue.
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-first-element queue) element)
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (assert (queue-p queue))
  (push element (queue-head queue))
  (when (null (queue-tail queue))
    (setf (queue-tail queue) (queue-head queue)))
  queue)


(defun queue-sort (queue predicate &optional (key #'identity))
  (assert (queue-p queue))
  (setf (queue-head queue) (sort (queue-head queue) predicate :key key)
        (queue-tail queue) (last (queue-head queue))))


(defun queue-sorted-insert (queue element predicate &optional (key #'identity))
  "Scan the queue comparing the elements of the queue
with ELEMENT until PREDICATE returns NIL, then insert
ELEMENT right before the last compared queue element."
  (assert (queue-p queue))
  (if (null (queue-head queue))
      (progn
        (push element (queue-head queue))
        (setf (queue-tail queue) (queue-head queue)))
      (progn
        (if (funcall predicate
                     (funcall key element)
                     (funcall key (queue-first-element queue)))
            (push element (queue-head queue))
            (do* ((curr-list (queue-head queue) next-list)
                  (next-list (cdr curr-list) (cdr curr-list))
                  (next-elem (car next-list) (car next-list))
                  end-flag)
                 (end-flag)
              (when (or (null next-elem)
                        (funcall predicate
                                 (funcall key element)
                                 (funcall key next-elem)))
                (setf end-flag t)
                (if (null next-elem)
                    ;; end of the queue has been reached
                    (setf (cdr curr-list) (list element)
                          (queue-tail queue) (cdr curr-list))
                    (let ((newcons (list element)))
                      (setf (cdr curr-list) newcons
                            (cdr newcons) next-list))))))))
  queue)


;; TODO: make it traverse the queue only once
(defun queue-filter (queue test &optional (key #'identity))
  "Delete from the queue all elements that satisfy TEST
and return them as list."
  (assert (queue-p queue))
  (remove-if-not test (queue-head queue) :key key))


;; TODO: make it traverse the queue only once
(defun queue-filter-and-delete (queue test &optional (key #'identity))
  "Delete from the queue all elements that satisfy TEST
and return them as list."
  (assert (queue-p queue))
  (prog1 (remove-if-not test (queue-head queue) :key key)
    (setf (queue-head queue) (delete-if test (queue-head queue) :key key)
          (queue-tail queue) (last (queue-head queue)))))


;;;; queue.lisp                       --                     --          ;;;;
