;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of strings.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun make-rod (size &key (initial-element 0))
  (check-type initial-element rune)
  (make-array size :element-type 'rune :initial-element initial-element))

(defun string-rod (string)
  (map 'rod #'char-rune (string string)))

(defun rod (thing &key new)
  (etypecase thing
    (rod
     (if new (copy-seq thing) thing))
    (rune
     (make-rod 1 :initial-element thing))
    ((or string symbol character)
     (string-rod thing))
    (vector
     (coerce thing 'rod))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun rodp (rod)
  (typep rod 'rod))

(defmacro with-rods ((&rest rods) &body body)
  `(let ,(loop :for rod :in rods
               :collect `(,(car rod) (rod ,(car rod))))
     ,@(loop :for (name start end) :in rods
             :collect `(check-bounds ,name ,start ,end))
     ,@body))

(defun rod= (rod1 rod2 &key (start1 0) end1 (start2 0) end2)
  (with-rods ((rod1 start1 end1)
              (rod2 start2 end2))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (loop :for i :from start1 :below end1
              :for j :from start2 :below end2
              :always (rune= (aref rod1 i)
                             (aref rod2 j))))))

(defun rod-equal (rod1 rod2 &key (start1 0) end1 (start2 0) end2)
  (with-rods ((rod1 start1 end1)
              (rod2 start2 end2))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (loop :for i :from start1 :below end1
              :for j :from start2 :below end2
              :always (rune-equal (aref rod1 i)
                                  (aref rod2 j))))))

(defun rod/= (rod1 rod2 &key (start1 0) end1 (start2 0) end2)
  (with-rods ((rod1 start1 end1)
              (rod2 start2 end2))
    (loop :for i :from start1 :below end1
          :for j :from start2 :below end2
          :when (rune/= (aref rod1 i)
                        (aref rod2 j))
          :do (return i)
          :finally (return (if (= (- end1 start1)
                                  (- end2 start2))
                               nil
                               i)))))

(defun rod-not-equal (rod1 rod2 &key (start1 0) end1 (start2 0) end2)
  (with-rods ((rod1 start1 end1)
              (rod2 start2 end2))
    (loop :for i :from start1 :below end1
          :for j :from start2 :below end2
          :when (rune-not-equal (aref rod1 i)
                                (aref rod2 j))
          :do (return i)
          :finally (return (if (= (- end1 start1)
                                  (- end2 start2))
                               nil
                               i)))))


(macrolet
    ((define-rod-comparison (name test equality &optional lessp equalp)
       `(defun ,name (rod1 rod2 &key (start1 0) end1 (start2 0) end2)
          (with-rods ((rod1 start1 end1) (rod2 start2 end2))
            (let ((len1 (- end1 start1))
                  (len2 (- end2 start2))
                  (index
                   (mismatch rod1 rod2 :test #',equality
                             :start1 start1 :end1 end1
                             :start2 start2 :end2 end2)))
              (cond
                ((null index) ,(if equalp 'end1 nil))
                ((= len1 len2) (if (,test (aref rod1 index)
                                          (aref rod2 (+ start2
                                                        (- index start1))))
                                   index
                                   nil))
                ((,(if lessp '< '>) len1 len2) index)))))))
  (define-rod-comparison rod<             rune<             rune=      t    )
  (define-rod-comparison rod-lessp        rune-lessp        rune-equal t    )
  (define-rod-comparison rod>             rune>             rune=           )
  (define-rod-comparison rod-greaterp     rune-greaterp     rune-equal      )
  (define-rod-comparison rod<=            rune<=            rune=      t   t)
  (define-rod-comparison rod-not-greaterp rune-not-greaterp rune-equal t   t)
  (define-rod-comparison rod>=            rune>=            rune=      nil t)
  (define-rod-comparison rod-not-lessp    rune-not-lessp    rune-equal nil t))


;;;-------------------------------------------------------------------------
;;; Operators
;;;-------------------------------------------------------------------------

(defun rod-string (rod &key (start 0) end)
  (check-bounds rod start end)
  ;; FIXME: inefficient
  (map 'string #'rune-char (subseq rod start end)))

(defun rod-upcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-upcase (rod rod :new t)
               :start start :end end))

(defun rod-downcase (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-downcase (rod rod :new t)
                 :start start :end end))

(defun rod-capitalize (rod &key (start 0) end)
  (check-bounds rod start end)
  (nrod-capitalize (rod rod :new t)
                   :start start :end end))

(defun nrod-upcase (rod &key (start 0) end)
  (check-type rod rod)
  (check-bounds rod start end)
  (loop :for i :from start :below end :do
        (setf (aref rod i)
              (rune-upcase (aref rod i))))
  rod)

(defun nrod-downcase (rod &key (start 0) end)
  (check-type rod rod)
  (check-bounds rod start end)
  (loop :for i :from start :below end :do
        (setf (aref rod i)
              (rune-downcase (aref rod i))))
  rod)

(defun nrod-capitalize (rod &key (start 0) end)
  (check-type rod rod)
  (check-bounds rod start end)
  (let ((i start))
    (labels ((%nupcase-rune (pos)
               (setf (aref rod pos) (rune-upcase (aref rod pos))))
             (%ndowncase-rune (pos)
               (setf (aref rod pos) (rune-downcase (aref rod pos))))
             (%ncapitalize-word ()
               (if-let ((pos (position-if #'alphanumeric-rune-p rod
                                          :start i :end end)))
                 (progn
                   (%nupcase-rune pos)
                   (setf i (1+ pos))
                   (loop :until (or (= i end)
                                    (not (alphanumeric-rune-p (aref rod i))))
                         :do (%ndowncase-rune i) (incf i)))
                 (setf i end))))
      (loop :until (= i end) :do (%ncapitalize-word))
      rod)))

(defun %rod-left-trim (rod rune-bag)
  (or (position-if-not (lambda (rune) (find rune rune-bag)) rod)
      (length rod)))

(defun %rod-right-trim (rod rune-bag)
  (or (position-if-not (lambda (rune) (find rune rune-bag)) rod
                       :from-end t)
      0))

(defun rod-trim (rod rune-bag)
  (let* ((rod (rod rod))
         (rune-bag (map 'rod #'rune rune-bag))
         (left  (%rod-left-trim  rod rune-bag))
         (right (%rod-right-trim rod rune-bag)))
    (subseq rod left (1+ right))))

(defun rod-left-trim (rod rune-bag)
  (let* ((rod (rod rod))
         (rune-bag (map 'rod #'rune rune-bag))
         (left (%rod-left-trim rod rune-bag)))
    (subseq rod left)))

(defun rod-right-trim (rod rune-bag &aux (rod (rod rod)))
  (let* ((rune-bag (map 'rod #'rune rune-bag))
         (right (%rod-right-trim rod rune-bag)))
    (subseq rod 0 (1+ right))))


;;;-------------------------------------------------------------------------
;;; Runes
;;;-------------------------------------------------------------------------

(defun name-rune (name)
  (let ((name (rod-downcase (rod name))))
    (if (and (= 23 (length name))
             (starts-with-subseq (rod "non-unicode rune #x") name))
        (let ((rune (parse-integer (rod-string name) :start 19 :radix 16)))
          (and (not (unicode-rune-p rune)) rune))
        (if-let (char (name-char (rod-string name)))
          (char-rune char)))))

;; FIXME: return rods ?
(defun rune-name (rune)
  (if (unicode-rune-p rune)
      (char-name (rune-char rune))
      (format nil "Non-Unicode rune #x~X" rune)))
