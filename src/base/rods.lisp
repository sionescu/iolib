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

(macrolet
    ((define-rod-comparison (name test &key (key 'identity))
       (with-gensyms (rod1 rod2 start1 start2 end1 end2 i j)
         `(defun ,name (,rod1 ,rod2 &key (,start1 0) ,end1 (,start2 0) ,end2)
            (check-type ,rod1 rod)
            (check-type ,rod2 rod)
            (check-bounds ,rod1 ,start1 ,end1)
            (check-bounds ,rod2 ,start2 ,end2)
            (loop :for ,i :from ,start1 :below ,end1
                  :for ,j :from ,start2 :below ,end2
                  :always (,test (,key (aref ,rod1 ,i))
                            (,key (aref ,rod2 ,j))))))))
  (define-rod-comparison rod=              =                   )
  (define-rod-comparison rod-equal         = :key rune-downcase)
  (define-rod-comparison rod/=            /=                   )
  (define-rod-comparison rod-not-equal    /= :key rune-downcase)
  (define-rod-comparison rod<              <                   )
  (define-rod-comparison rod-lessp         < :key rune-downcase)
  (define-rod-comparison rod>              >                   )
  (define-rod-comparison rod-greaterp      > :key rune-downcase)
  (define-rod-comparison rod<=            <=                   )
  (define-rod-comparison rod-not-greaterp <= :key rune-downcase)
  (define-rod-comparison rod>=            >=                   )
  (define-rod-comparison rod-not-lessp    >= :key rune-downcase))


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
      0))

(defun %rod-right-trim (rod rune-bag)
  (or (position-if-not (lambda (rune) (find rune rune-bag)) rod
                       :from-end t)
      (length rod)))

(defun rod-trim (rod rune-bag)
  (check-type rod rod)
  (assert (every #'runep rune-bag))
  (let ((left  (%rod-left-trim  rod rune-bag))
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
