;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- SPLIT-SEQUENCE
;;;
;;; This code was based on Arthur Lemmens' in
;;; <URL:http://groups.google.com/groups?as_umsgid=39F36F1A.B8F19D20%40simplex.nl>;
;;;
;;; Examples:
;;;
;;; * (split-sequence #\; "a;;b;c")
;;; -> ("a" "" "b" "c"), 6
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t)
;;; -> ("a" "" "b" "c"), 0
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t :count 1)
;;; -> ("c"), 4
;;;
;;; * (split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
;;; -> ("a" "b" "c"), 6
;;;
;;; * (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("" "" "r" "c" "d" "" "r" ""), 11
;;;
;;; * (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("ab" "a" "a" "ab" "a"), 11
;;;
;;; * (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
;;; -> ("oo" "bar" "b"), 9

(in-package :iolib.base)

(defun split-sequence (delimiter seq &key (start 0) (end nil) (from-end nil)
                       (count nil) (remove-empty-subseqs nil)
                       (test #'eql) (test-not nil) (key #'identity))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq)))
    (unless end (setf end len))
    (cond
      ((and (not from-end) (null test-not))
       (split-from-start (lambda (start)
                            (position delimiter seq :start start :key key :test test))
                          seq len start end count remove-empty-subseqs))
      ((and (not from-end) test-not)
       (split-from-start (lambda (start)
                            (position delimiter seq :start start :key key :test-not test-not))
                          seq len start end count remove-empty-subseqs))
      ((and from-end (null test-not))
       (split-from-end (lambda (end)
                         (position delimiter seq :end end :from-end t :key key :test test))
                       seq start end count remove-empty-subseqs))
      ((and from-end test-not)
       (split-from-end (lambda (end)
                          (position delimiter seq :end end :from-end t :key key :test-not test-not))
                        seq start end count remove-empty-subseqs)))))

(defun split-sequence-if (predicate seq &key (start 0) (end nil) (from-end nil)
                          (count nil) (remove-empty-subseqs nil) (key #'identity))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq)))
    (unless end (setf end len))
    (if from-end
        (split-from-end (lambda (end)
                          (position-if predicate seq :end end :from-end t :key key))
                        seq start end count remove-empty-subseqs)
        (split-from-start (lambda (start)
                            (position-if predicate seq :start start :key key))
                          seq len start end count remove-empty-subseqs))))

(defun split-sequence-if-not (predicate seq &key (count nil) (remove-empty-subseqs nil)
                              (from-end nil) (start 0) (end nil) (key #'identity))
  "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq)))
    (unless end (setf end len))
    (if from-end
        (split-from-end (lambda (end)
                          (position-if-not predicate seq :end end :from-end t :key key))
                        seq start end count remove-empty-subseqs)
        (split-from-start (lambda (start)
                            (position-if-not predicate seq :start start :key key))
                          seq len start end count remove-empty-subseqs))))

(defun split-from-end (position-fn seq start end count remove-empty-subseqs)
  (loop
     :for right := end :then left
     :for left := (max (or (funcall position-fn right) -1)
                       (1- start))
     :unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       :return (values (nreverse subseqs) right)
     :else
       :collect (subseq seq (1+ left) right) into subseqs
       :and :sum 1 :into nr-elts
     :until (< left start)
     :finally (return (values (nreverse subseqs) (1+ left)))))

(defun split-from-start (position-fn seq len start end count remove-empty-subseqs)
  (loop
     :for left := start :then (+ right 1)
     :for right := (min (or (funcall position-fn left) len)
                        end)
     :unless (and (= right left)
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       :return (values subseqs left)
     :else
       :collect (subseq seq left right) :into subseqs
       :and :sum 1 :into nr-elts
     :until (>= right end)
     :finally (return (values subseqs right))))
