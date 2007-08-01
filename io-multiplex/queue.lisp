;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; queue.lisp --- FIFO-optimized queues.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2001-2005, Pascal J. Bourguignon  <pjb@informatimago.com>
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

(in-package :io.multiplex)

;;; The structure of a queue is as follows:
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

;;; Returns the number of elements in the queue.
(defun queue-length (queue)
  (assert (queue-p queue))
  (length (queue-head queue))) ;;queue-length

(defun queue-empty-p (queue)
  (assert (queue-p queue))
  (null (queue-head queue)))

;;; Returns the first element of the queue.
(defun queue-first-element (queue)
  (assert (queue-p queue))
  (first (queue-head queue)))

;;; Returns the last element of the queue.
(defun queue-last-element (queue)
  (assert (queue-p queue))
  (first (queue-tail queue)))

(defun queue-enqueue  (queue element)
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
  (assert (queue-p queue))
  (prog1 (pop (queue-head queue))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))))

;;; Insert the element at the beginning of the queue.
(defun queue-requeue (queue element)
  (assert (queue-p queue))
  (push element (queue-head queue))
  (when (null (queue-tail queue))
    (setf (queue-tail queue) (queue-head queue)))
  queue)

(defun queue-sort (queue predicate &optional (key #'identity))
  (assert (queue-p queue))
  (setf (queue-head queue) (sort (queue-head queue) predicate :key key)
        (queue-tail queue) (last (queue-head queue))))

;;; Scan the queue comparing the elements of the queue with ELEMENT
;;; until PREDICATE returns NIL, then insert ELEMENT right before the
;;; last compared queue element.
(defun queue-sorted-insert (queue element predicate &optional (key #'identity))
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

;;; TODO: make it traverse the queue only once
;;;
;;; Delete from the queue all elements that satisfy TEST and return
;;; them as list.
(defun queue-filter (queue test &optional (key #'identity))
  (assert (queue-p queue))
  (remove-if-not test (queue-head queue) :key key))

;;; TODO: make it traverse the queue only once
;;;
;;; Delete from the queue all elements that satisfy TEST and return
;;; them as list.
(defun queue-filter-and-delete (queue test &optional (key #'identity))
  (assert (queue-p queue))
  (prog1 (remove-if-not test (queue-head queue) :key key)
    (setf (queue-head queue) (delete-if test (queue-head queue) :key key)
          (queue-tail queue) (last (queue-head queue)))))
