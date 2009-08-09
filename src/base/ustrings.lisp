;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of strings.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun make-ustring (size &key (initial-element 0))
  (check-type initial-element uchar)
  (make-array size :element-type 'uchar :initial-element initial-element))

(define-compiler-macro make-ustring (&whole whole size &key (initial-element 0))
  (cond
    ((and (constantp size) (constantp initial-element))
     (let ((sz (eval size))
           (el (eval initial-element)))
       (assert (typep el 'uchar))
       (make-array sz :element-type 'uchar :initial-element el)))
    (t whole)))

(defun string-to-ustring (string)
  (map 'simple-ustring #'char-to-uchar (string string)))

(define-compiler-macro string-to-ustring (&whole whole string)
  (if (constantp string)
      (map 'simple-ustring #'char-to-uchar (string (eval string)))
      whole))

(defun %ustring (thing new)
  (etypecase thing
    (ustring
     (if new (copy-seq thing) thing))
    (string-designator
     (string-to-ustring thing))
    (uchar
     (make-ustring 1 :initial-element thing))
    (vector
     (coerce thing 'simple-ustring))))

(defun ustring (thing &key new)
  (%ustring thing new))

(define-compiler-macro ustring (&whole whole thing &key new)
  (cond
    ((and (constantp thing) (constantp new))
     (%ustring (eval thing) (eval new)))
    (t whole)))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun ustringp (ustring)
  (typep ustring 'ustring))

(defun simple-ustring-p (ustring)
  (typep ustring 'simple-ustring))

(defun complex-ustring-p (ustring)
  (typep ustring 'complex-ustring))

(defmacro with-ustrings ((&rest ustrings) &body body)
  `(let ,(loop :for ustring :in ustrings
               :collect `(,(car ustring) (ustring ,(car ustring))))
     ,@(loop :for (name start end) :in ustrings
             :collect `(check-bounds ,name ,start ,end))
     ,@body))

(defun ustring= (ustring1 ustring2 &key (start1 0) end1 (start2 0) end2)
  (with-ustrings ((ustring1 start1 end1)
                  (ustring2 start2 end2))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (loop :for i :from start1 :below end1
              :for j :from start2 :below end2
              :always (uchar= (aref ustring1 i)
                              (aref ustring2 j))))))

(defun ustring-equal (ustring1 ustring2 &key (start1 0) end1 (start2 0) end2)
  (with-ustrings ((ustring1 start1 end1)
                  (ustring2 start2 end2))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (loop :for i :from start1 :below end1
              :for j :from start2 :below end2
              :always (uchar-equal (aref ustring1 i)
                                   (aref ustring2 j))))))

(defun ustring/= (ustring1 ustring2 &key (start1 0) end1 (start2 0) end2)
  (with-ustrings ((ustring1 start1 end1)
                  (ustring2 start2 end2))
    (loop :for i :from start1 :below end1
          :for j :from start2 :below end2
          :when (uchar/= (aref ustring1 i)
                         (aref ustring2 j))
          :do (return i)
          :finally (return (if (= (- end1 start1)
                                  (- end2 start2))
                               nil
                               i)))))

(defun ustring-not-equal (ustring1 ustring2 &key (start1 0) end1 (start2 0) end2)
  (with-ustrings ((ustring1 start1 end1)
                  (ustring2 start2 end2))
    (loop :for i :from start1 :below end1
          :for j :from start2 :below end2
          :when (uchar-not-equal (aref ustring1 i)
                                 (aref ustring2 j))
          :do (return i)
          :finally (return (if (= (- end1 start1)
                                  (- end2 start2))
                               nil
                               i)))))

(macrolet
    ((define-ustring-comparison (name test equality lessp equalp)
       `(defun ,name (ustring1 ustring2 &key (start1 0) end1 (start2 0) end2)
          (with-ustrings ((ustring1 start1 end1) (ustring2 start2 end2))
            (let ((len1 (- end1 start1))
                  (len2 (- end2 start2))
                  (index
                   (mismatch ustring1 ustring2 :test #',equality
                             :start1 start1 :end1 end1
                             :start2 start2 :end2 end2)))
              (cond
                ((null index) ,(if equalp 'end1 nil))
                ((= len1 len2) (if (,test (aref ustring1 index)
                                          (aref ustring2 (+ start2
                                                        (- index start1))))
                                   index
                                   nil))
                ((,(if lessp '< '>) len1 len2) index)))))))
  (define-ustring-comparison ustring<             uchar<             uchar=      t   nil)
  (define-ustring-comparison ustring-lessp        uchar-lessp        uchar-equal t   nil)
  (define-ustring-comparison ustring>             uchar>             uchar=      nil nil)
  (define-ustring-comparison ustring-greaterp     uchar-greaterp     uchar-equal nil nil)
  (define-ustring-comparison ustring<=            uchar<=            uchar=      t   t)
  (define-ustring-comparison ustring-not-greaterp uchar-not-greaterp uchar-equal t   t)
  (define-ustring-comparison ustring>=            uchar>=            uchar=      nil t)
  (define-ustring-comparison ustring-not-lessp    uchar-not-lessp    uchar-equal nil t))


;;;-------------------------------------------------------------------------
;;; Operators
;;;-------------------------------------------------------------------------

(defun ustring-to-string (ustring &key (start 0) end)
  (check-type ustring ustring)
  (check-bounds ustring start end)
  ;; FIXME: inefficient
  (map 'string #'uchar-to-char (subseq ustring start end)))

(defun ustring-to-string* (ustring &key (start 0) end)
  (check-type ustring ustring)
  (check-bounds ustring start end)
  (let ((string (make-string (- end start))))
    (loop :for i :from start :below end
          :for uchar := (aref ustring i)
          :for sindex :from 0 :do
          (setf (char string sindex)
                (uchar-to-char
                 (cond
                   ((<= #xD800   uchar #xDFFF)   (logand uchar #x7FF))
                   ((<= #x110000 uchar #x1100FF) (logand uchar #xFF))
                   (t                            uchar)))))
    string))

(defun ustring-upcase (ustring &key (start 0) end)
  (check-bounds ustring start end)
  (nustring-upcase (ustring ustring :new t)
                   :start start :end end))

(defun ustring-downcase (ustring &key (start 0) end)
  (check-bounds ustring start end)
  (nustring-downcase (ustring ustring :new t)
                     :start start :end end))

(defun ustring-capitalize (ustring &key (start 0) end)
  (check-bounds ustring start end)
  (nustring-capitalize (ustring ustring :new t)
                       :start start :end end))

(defun nustring-upcase (ustring &key (start 0) end)
  (check-type ustring ustring)
  (check-bounds ustring start end)
  (loop :for i :from start :below end :do
        (setf (aref ustring i)
              (uchar-upcase (aref ustring i))))
  ustring)

(defun nustring-downcase (ustring &key (start 0) end)
  (check-type ustring ustring)
  (check-bounds ustring start end)
  (loop :for i :from start :below end :do
        (setf (aref ustring i)
              (uchar-downcase (aref ustring i))))
  ustring)

(defun nustring-capitalize (ustring &key (start 0) end)
  (check-type ustring ustring)
  (check-bounds ustring start end)
  (let ((i start))
    (labels ((%nupcase-uchar (pos)
               (setf (aref ustring pos) (uchar-upcase (aref ustring pos))))
             (%ndowncase-uchar (pos)
               (setf (aref ustring pos) (uchar-downcase (aref ustring pos))))
             (%ncapitalize-word ()
               (if-let ((pos (position-if #'alphanumeric-uchar-p ustring
                                          :start i :end end)))
                 (progn
                   (%nupcase-uchar pos)
                   (setf i (1+ pos))
                   (loop :until (or (= i end)
                                    (not (alphanumeric-uchar-p (aref ustring i))))
                         :do (%ndowncase-uchar i) (incf i)))
                 (setf i end))))
      (loop :until (= i end) :do (%ncapitalize-word))
      ustring)))

(defun %ustring-left-trim (ustring uchar-bag)
  (or (position-if-not (lambda (uchar) (find uchar uchar-bag)) ustring)
      (length ustring)))

(defun %ustring-right-trim (ustring uchar-bag)
  (or (position-if-not (lambda (uchar) (find uchar uchar-bag)) ustring
                       :from-end t)
      0))

(defun ustring-trim (ustring uchar-bag)
  (let* ((ustring (ustring ustring))
         (uchar-bag (map 'simple-ustring #'uchar uchar-bag))
         (left  (%ustring-left-trim  ustring uchar-bag))
         (right (%ustring-right-trim ustring uchar-bag)))
    (subseq ustring left (1+ right))))

(defun ustring-left-trim (ustring uchar-bag)
  (let* ((ustring (ustring ustring))
         (uchar-bag (map 'simple-ustring #'uchar uchar-bag))
         (left (%ustring-left-trim ustring uchar-bag)))
    (subseq ustring left)))

(defun ustring-right-trim (ustring uchar-bag &aux (ustring (ustring ustring)))
  (let* ((uchar-bag (map 'simple-ustring #'uchar uchar-bag))
         (right (%ustring-right-trim ustring uchar-bag)))
    (subseq ustring 0 (1+ right))))


;;;-------------------------------------------------------------------------
;;; Uchars
;;;-------------------------------------------------------------------------

(defun name-uchar (name)
  (let ((name (ustring-downcase (ustring name))))
    (if (and (= 24 (length name))
             (starts-with-subseq (ustring-upcase "Non-Unicode uchar #x")
                                 (ustring-upcase name)))
        (let ((uchar (parse-integer (ustring-to-string name) :start 20 :radix 16)))
          (and (not (unicode-uchar-p uchar)) uchar))
        (if-let (char (name-char (ustring-to-string name)))
          (char-to-uchar char)))))

;; FIXME: return ustrings ?
(defun uchar-name (uchar)
  (if (unicode-uchar-p uchar)
      (char-name (uchar-to-char uchar))
      (format nil "Non-Unicode uchar #x~X" uchar)))


;;;-------------------------------------------------------------------------
;;; Misc
;;;-------------------------------------------------------------------------

(defun join/ustring (connector &rest ustrings)
  (let ((c (ustring connector)))
    (concatenate 'simple-ustring (car ustrings)
                 (reduce (lambda (str1 str2)
                           (concatenate 'simple-ustring str1 c str2))
                         (cdr ustrings)
                         :initial-value (make-ustring 0)))))
