;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of strings.
;;;

(in-package :iolib.pathnames)

;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun make-rod (size &key (initial-element 0))
  (check-type initial-element rune)
  (make-array size :initial-element initial-element))

(defun rod (thing)
  (etypecase thing
    (rod              thing)
    ((array rune (*)) (coerce thing 'rod))
    (string           (map 'rod #'char-rune thing))
    (rune             (make-rod 1 :initial-element thing))
    (character        (make-rod 1 :initial-element (char-rune thing)))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun rodp (rod)
  (typep rod 'rod))

(defun rod= (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod/= (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod< (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod> (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod<= (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod>= (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-equal (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-not-equal (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-lessp (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-greaterp (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-not-greaterp (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))

(defun rod-not-lessp (rod1 rod2 &key start1 end1 start2 end2)
  (check-type rod1 rod)
  (check-type rod2 rod)
  (check-bounds rod1 start1 end1)
  (check-bounds rod2 start2 end2))


;;;-------------------------------------------------------------------------
;;; Operators
;;;-------------------------------------------------------------------------

(defun rod-upcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-upcase (copy-seq rod)
               :start start :end end))

(defun rod-downcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-downcase (copy-seq rod)
                 :start start :end end))

(defun rod-capitalize (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-capitalize (copy-seq rod)
                   :start start :end end))

(defun nrod-upcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (loop :for i :from start :below end :do
        (setf (aref rod i)
              (rune-upcase (aref rod i)))))

(defun nrod-downcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (loop :for i :from start :below end :do
        (setf (aref rod i)
              (rune-downcase (aref rod i)))))

(defun nrod-capitalize (rod &key (start 0) end)
  (check-bounds rod start end)
  ;; TODO: Implement it
  )

(defun %rod-left-trim (rod rune-bag)
  (or (position-if-not (lambda (rune) (find rune rune-bag)) rod)
      0))

(defun %rod-right-trim (rod rune-bag)
  (or (position-if-not (lambda (rune) (find rune rune-bag)) rod
                       :from-end t)
      (length rod)))

(defun rod-trim (rod rune-bag)
  (check-type rod rod)
  (assert (every #'runep rune-bag))
  (let ((left  (%rod-left-trim rod rune-bag))
        (right (%rod-right-trim rod rune-bag)))
    (if (and (zerop left)
             (= right (length rod)))
        rod
        (subseq rod left right))))

(defun rod-left-trim (rod rune-bag)
  (check-type rod rod)
  (assert (every #'runep rune-bag))
  (let ((left (%rod-left-trim rod rune-bag)))
    (if (zerop left)
        rod
        (subseq rod left))))

(defun rod-right-trim (rod rune-bag)
  (check-type rod rod)
  (assert (every #'runep rune-bag))
  (let ((right (%rod-right-trim rod rune-bag)))
    (if (= right (length rod))
        rod
        (subseq rod 0 right))))

