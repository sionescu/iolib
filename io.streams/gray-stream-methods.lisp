;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; gray-stream-methods.lisp --- Implementation using gray streams.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :io.streams)

;;;; Instance Initialization

;;; TODO: use the buffer pool
;;; TODO: handle instance reinitialization
(defmethod shared-initialize :after ((stream dual-channel-gray-stream) slot-names
                                     &key (input-buffer-size +bytes-per-iobuf+)
                                     (output-buffer-size +bytes-per-iobuf+)
                                     (external-format :default))
  (declare (ignore slot-names))
  (unless input-buffer-size (setf input-buffer-size +bytes-per-iobuf+))
  (unless output-buffer-size (setf output-buffer-size +bytes-per-iobuf+))
  (check-type input-buffer-size buffer-index)
  (check-type output-buffer-size buffer-index)
  (with-accessors ((ib input-buffer-of)
                   (ob output-buffer-of)
                   (ef external-format-of))
      stream
    (setf ib (allocate-iobuf input-buffer-size)
          ob (allocate-iobuf output-buffer-size)
          ef external-format)))

;;;; Common Methods

(defmethod stream-element-type ((stream dual-channel-gray-stream))
  '(unsigned-byte 8))

;; TODO: use the buffer pool
(defmethod close :around ((stream dual-channel-gray-stream) &key abort)
  (with-accessors ((ib input-buffer-of)
                   (ob output-buffer-of))
      stream
    (unless (or abort (null ib)) (finish-output stream))
    (when ib (free-iobuf ib))
    (when ob (free-iobuf ob))
    (setf ib nil ob nil))
  (call-next-method)
  (values stream))

(defmethod close ((stream dual-channel-gray-stream) &key abort)
  (declare (ignore stream abort)))

(defmethod (setf external-format-of)
    (external-format (stream dual-channel-gray-stream))
  (setf (slot-value stream 'external-format)
        (babel:ensure-external-format external-format)))

;;;; Input Methods

(defun %to-octets (buff start end ef)
  (babel:string-to-octets buff :start start :end end
                          :encoding (babel:external-format-encoding ef)))

(defmethod stream-clear-input ((stream dual-channel-gray-stream))
  (with-accessors ((ib input-buffer-of))
      stream
    (iobuf-reset ib)
    nil))

(defun %fill-ibuf (read-fn fd buf &optional timeout)
  (when timeout
    (let ((readablep (iomux:wait-until-fd-ready fd :read timeout)))
      (unless readablep
        (return-from %fill-ibuf :timeout))))
  (let ((num (nix:repeat-upon-eintr
               (funcall read-fn fd (iobuf-end-pointer buf)
                        (iobuf-end-space-length buf)))))
    (if (zerop num)
        :eof
        (incf (iobuf-end buf) num))))

(defun %read-into-simple-array-ub8 (stream array start end)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (fd input-fd-of)
                   (read-fn read-fn-of))
      stream
    (let ((octets-needed (- end start)))
      (loop :with array-offset := start
            :for octets-in-buffer := (iobuf-length ib)
            :for nbytes := (min octets-needed octets-in-buffer)
         :when (plusp nbytes) :do
           (iobuf-copy-into-lisp-array ib (iobuf-start ib)
                                       array array-offset nbytes)
           (incf array-offset nbytes)
           (decf octets-needed nbytes)
           (incf (iobuf-start ib) nbytes)
         :if (zerop octets-needed) :do (loop-finish)
         :else :do (iobuf-reset ib)
         :when (eq :eof (%fill-ibuf read-fn fd ib)) :do (loop-finish)
         :finally (return array-offset)))))

(defun %read-into-string (stream string start end)
  (declare (type dual-channel-gray-stream stream))
  (loop :for offset :from start :below end
        :for char := (stream-read-char stream)
     :if (eq char :eof) :do (loop-finish)
     :else :do (setf (char string offset) char)
     :finally (return offset)))

(defun %read-into-vector (stream vector start end)
  (declare (type dual-channel-gray-stream stream))
  (loop :for offset :from start :below end
        :for octet := (stream-read-byte stream)
     :if (eq octet :eof) :do (loop-finish)
     :else :do (setf (aref vector offset) octet)
     :finally (return offset)))

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (unless ,end
         (setq ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(declaim (inline %read-sequence))
(defun %read-sequence (stream seq start end)
  (check-bounds seq start end)
  (when (< start end)
    (etypecase seq
      (ub8-sarray (%read-into-simple-array-ub8 stream seq start end))
      (string (%read-into-string stream seq start end))
      (ub8-vector (%read-into-vector stream seq start end)))))

(declaim (inline read-sequence*))
(defun read-sequence* (stream sequence &key (start 0) end)
  (%read-sequence stream sequence start end))

(defmethod stream-read-sequence
    ((stream dual-channel-gray-stream) sequence start end &key)
  (%read-sequence stream sequence start end))

;;;; Output Methods

(defun %write-n-bytes (write-fn fd buf nbytes &optional timeout)
  (declare (type stream-buffer buf))
  (let ((bytes-written 0))
    (labels ((write-once ()
               (let ((num (handler-case
                              (nix:repeat-upon-condition-decreasing-timeout
                                  ((nix:eintr) timeout-var timeout)
                                (prog1
                                    (funcall write-fn fd (inc-pointer buf bytes-written)
                                             nbytes)
                                  (when (and timeout-var (zerop timeout-var))
                                    (return-from %write-n-bytes
                                      (values nil :timeout)))))
                            (nix:epipe ()
                              (return-from %write-n-bytes (values nil :eof))))))
                 (unless (zerop num) (incf bytes-written num))))
             (write-or-return ()
               (unless (write-once)
                 (when (errorp)
                   ;; FIXME signal something better -- maybe analyze the status
                   (return-from %write-n-bytes (values nil :fail)))))
             (buffer-emptyp () (= bytes-written nbytes))
             (errorp () (handler-case (iomux:wait-until-fd-ready fd :write)
                          (iomux:poll-error () t)
                          (:no-error (r w) (declare (ignore r w)) nil))))
      (loop :until (buffer-emptyp) :do (write-or-return)
         :finally (return (values t bytes-written))))))

(defun %flush-obuf (write-fn fd buf &optional timeout)
  (declare (type iobuf buf))
  (let ((bytes-written 0))
    (labels ((write-once ()
               (let ((num (handler-case
                              (nix:repeat-upon-condition-decreasing-timeout
                                  ((nix:eintr) timeout-var timeout)
                                (prog1
                                    (funcall write-fn fd (iobuf-start-pointer buf)
                                             (iobuf-length buf))
                                  (when (and timeout-var (zerop timeout-var))
                                    (return-from %flush-obuf
                                      (values nil :timeout)))))
                            (nix:epipe ()
                              (return-from %flush-obuf (values nil :eof))))))
                 (unless (zerop num)
                   (incf (iobuf-start buf) num)
                   (incf bytes-written num))))
             (write-or-return ()
               (unless (write-once)
                 (when (errorp)
                   ;; FIXME signal something better -- maybe analyze the status
                   (return-from %flush-obuf (values nil :fail)))))
             (buffer-emptyp ()
               (when (iobuf-empty-p buf)
                 (iobuf-reset buf) t))
             (errorp () (handler-case (iomux:wait-until-fd-ready fd :write)
                          (iomux:poll-error () t)
                          (:no-error (r w) (declare (ignore r w)) nil))))
      (loop :until (buffer-emptyp) :do (write-or-return)
         :finally (return (values t bytes-written))))))

;;; TODO: add timeout support
(defun %flush-obuf-if-needed (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((fd output-fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (when (or dirtyp (iobuf-full-p ob))
      (%flush-obuf write-fn fd ob)
      (setf dirtyp nil))))

(defmethod stream-clear-output ((stream dual-channel-gray-stream))
  (with-accessors ((ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (iobuf-reset ob)
    (setf dirtyp nil)
    nil))

(defmethod stream-finish-output ((stream dual-channel-gray-stream))
  (with-accessors ((fd output-fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (%flush-obuf write-fn fd ob)
    (setf dirtyp nil)
    nil))

(defmethod stream-force-output ((stream dual-channel-gray-stream))
  (setf (dirtyp stream) t))

(defun %write-simple-array-ub8 (stream array start end)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((fd output-fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of))
      stream
    (let ((octets-needed (- end start)))
      (cond ((<= octets-needed (iobuf-end-space-length ob))
             (iobuf-copy-from-lisp-array array start ob
                                         (iobuf-end ob) octets-needed)
             (incf (iobuf-end ob) octets-needed)
             (%flush-obuf-if-needed stream))
            (t 
             (with-pointer-to-vector-data (ptr array)
               (%flush-obuf write-fn fd ob)
               (let ((ret (%write-n-bytes write-fn fd (inc-pointer ptr start)
                                          octets-needed)))
                 (when (numberp ret)
                   (incf (iobuf-end ob) octets-needed))))))
      (values array))))

(defun %write-vector-ub8 (stream vector start end)
  (declare (type dual-channel-gray-stream stream))
  (%write-simple-array-ub8 stream (coerce vector 'ub8-sarray) start end))

(defun %write-vector (stream vector start end)
  (declare (type dual-channel-gray-stream stream))
  (loop :for offset :from start :below end
        :for octet := (aref vector offset)
     :do (stream-write-byte stream octet)
     :finally (return vector)))

(declaim (inline %write-sequence))
(defun %write-sequence (stream seq start end)
  (check-bounds seq start end)
  (when (< start end)
    (etypecase seq
      (ub8-sarray (%write-simple-array-ub8 stream seq start end))
      (string (stream-write-string stream seq start end))
      (ub8-vector (%write-vector-ub8 stream seq start end))
      (vector (%write-vector stream seq start end)))))

(declaim (inline write-sequence*))
(defun write-sequence* (stream sequence &key (start 0) end)
  (%write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream dual-channel-gray-stream)
                                  sequence start end &key)
  (%write-sequence stream sequence start end))

;;;; Character Input

(defun maybe-find-line-ending (read-fn fd ib ef)
  (let* ((start-off (iobuf-start ib))
         (char-code (bref ib start-off)))
    (block nil
      (ecase (babel:external-format-eol-style ef)
        (:lf (when (= char-code (char-code #\Linefeed))
               (incf (iobuf-start ib))
               (return #\Newline)))
        (:cr (when (= char-code (char-code #\Return))
               (incf (iobuf-start ib))
               (return #\Newline)))
        (:crlf (when (= char-code (char-code #\Return))
                 (when (and (= (iobuf-length ib) 1)
                            (eq :eof (%fill-ibuf read-fn fd ib)))
                   (incf (iobuf-start ib))
                   (return #\Return))
                 (when (= (bref ib (1+ start-off))
                          (char-code #\Linefeed))
                   (incf (iobuf-start ib) 2)
                   (return #\Newline))))))))

(defconstant +max-octets-per-char+ 6)

;;; FIXME: currently we return :EOF when read(2) returns 0
;;;        we should distinguish hard end-of-files (EOF and buffer empty)
;;;        from soft end-of-files (EOF and *some* bytes still in the buffer
;;;        but not enough to make a full character)
(defmethod stream-read-char ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (ib input-buffer-of)
                   (read-fn read-fn-of)
                   (unread-index ibuf-unread-index-of)
                   (ef external-format-of))
      stream
    (setf unread-index (iobuf-start ib))
    (let ((str nil)
          (ret nil))
      (flet ((fill-buf-or-eof ()
               (setf ret (%fill-ibuf read-fn fd ib))
               (when (eq ret :eof)
                 (return-from stream-read-char :eof))))
        (cond ((zerop (iobuf-length ib))
               (iobuf-reset ib)
               (fill-buf-or-eof))
              ;; Some encodings such as CESU or Java's modified UTF-8 take
              ;; as much as 6 bytes per character. Make sure we have enough
              ;; space to collect read-ahead bytes if required.
              ((< (iobuf-length ib) +max-octets-per-char+)
               (iobuf-copy-data-to-start ib)
               (setf unread-index 0)))
        ;; line-end handling
        (when-let ((it (maybe-find-line-ending read-fn fd ib ef)))
          (return-from stream-read-char it))
        (tagbody :start
           (handler-case
               (setf (values str ret)
                     (foreign-string-to-lisp
                      (iobuf-data ib)
                      :offset (iobuf-start ib)
                      :count (iobuf-length ib)
                      :encoding (babel:external-format-encoding ef)
                      :max-chars 1))
             (babel:end-of-input-in-character ()
               (fill-buf-or-eof)
               (go :start)))
           (incf (iobuf-start ib) ret))
        (char str 0)))))

(defun maybe-find-line-ending-no-hang (fd ib ef)
  (declare (ignore fd))
  (let* ((start-off (iobuf-start ib))
         (char-code (bref ib start-off)))
    (block nil
      (ecase (babel:external-format-eol-style ef)
        (:lf (when (= char-code (char-code #\Linefeed))
               (incf (iobuf-start ib))
               (return #\Newline)))
        (:cr (when (= char-code (char-code #\Return))
               (incf (iobuf-start ib))
               (return #\Newline)))
        (:crlf (when (= char-code (char-code #\Return))
                 (when (= (iobuf-length ib) 1)
                   (incf (iobuf-start ib))
                   (return :starvation))
                 (when (= (bref ib (1+ start-off))
                          (char-code #\Linefeed))
                   (incf (iobuf-start ib) 2)
                   (return #\Newline))))))))

(defmethod stream-read-char-no-hang ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of)
                   (ef external-format-of))
      stream
    (let ((str nil)
          (ret nil)
          (eof nil))
      (block nil
        ;; BUG: this comparision is probably buggy, FIXME.  A similar
        ;; bug was fixed in STREAM-READ-CHAR.  Must write a test for
        ;; this one first.
        (when (< 0 (iobuf-end-space-length ib) 4)
          (iobuf-copy-data-to-start ib))
        (when (and (iomux:fd-ready-p fd :read)
                   (eq :eof (%fill-ibuf read-fn fd ib)))
          (setf eof t))
        (when (zerop (iobuf-length ib))
          (return (if eof :eof nil)))
        ;; line-end handling
        (let ((line-end (maybe-find-line-ending-no-hang fd ib ef)))
          (cond ((eq line-end :starvation)
                 (return (if eof #\Return nil)))
                ((characterp line-end)
                 (return line-end))))
        ;; octet decoding
        (handler-case
            (setf (values str ret)
                  (foreign-string-to-lisp
                   (iobuf-data ib)
                   :offset (iobuf-start ib)
                   :count (iobuf-length ib)
                   :encoding (babel:external-format-encoding ef)
                   :max-chars 1))
          (babel:end-of-input-in-character ()
            (return nil)))
        (incf (iobuf-start ib) ret)
        (char str 0)))))

(defun %stream-unread-char (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (unread-index ibuf-unread-index-of))
      stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index) (setf start unread-index))
        (t (error "No uncommitted character to unread")))))
  nil)

(defmethod stream-unread-char ((stream dual-channel-gray-stream) character)
  (declare (ignore character))
  (%stream-unread-char stream))

(defmethod stream-peek-char ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char stream)))
    (cond ((eq char :eof) :eof)
          (t (%stream-unread-char stream)
             (values char)))))

;; (defmethod stream-read-line ((stream dual-channel-gray-stream))
;;   )

(defmethod stream-listen ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((characterp char) (stream-unread-char stream char) t)
          ((eq char :eof) nil)
          (t t))))

;;;; Character Output

(defmethod stream-write-char ((stream dual-channel-gray-stream)
                              (character character))
  (%flush-obuf-if-needed stream)
  (if (char= character #\Newline)
      (%write-line-terminator
       stream (babel:external-format-eol-style (external-format-of stream)))
      ;; FIXME: avoid consing a string here. At worst, declare it dynamic-extent
      (stream-write-string stream (make-string 1 :initial-element character))))

(defmethod stream-line-column ((stream dual-channel-gray-stream))
  0)

(defmethod stream-start-line-p ((stream dual-channel-gray-stream))
  (values nil))

(defmethod stream-terpri ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) nil)

(defmethod stream-fresh-line ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) t)

(define-constant +unix-line-terminator+
    (make-array 1 :element-type 'ub8 :initial-contents '(10))
  :test 'equalp)

(define-constant +dos-line-terminator+
    (make-array 2 :element-type 'ub8 :initial-contents '(13 10))
  :test 'equalp)

(define-constant +mac-line-terminator+
    (make-array 1 :element-type 'ub8 :initial-contents '(13))
  :test 'equalp)

(defun %write-line-terminator (stream line-terminator)
  (case line-terminator
    (:lf (%write-simple-array-ub8 stream +unix-line-terminator+ 0 1))
    (:cr (%write-simple-array-ub8 stream +mac-line-terminator+ 0 1))
    (:crlf (%write-simple-array-ub8 stream +dos-line-terminator+ 0 2))))

(defmethod stream-write-string ((stream dual-channel-gray-stream)
                                (string string) &optional (start 0) end)
  (check-bounds string start end)
  (when (< start end)
    (let* ((octets nil)
           (ef (external-format-of stream))
           (line-terminator (babel:external-format-eol-style ef)))
      (loop :for off1 := start :then (1+ off2)
            :for nl-off := (position #\Newline string :start off1)
            :for off2 := (or nl-off end)
         :when nl-off :do (%write-line-terminator stream line-terminator)
         :when (> off2 off1) :do
         ;; FIXME: should probably convert directly to a foreign buffer?
         (setf octets (%to-octets string off1 off2 ef))
         (%write-simple-array-ub8 stream octets 0 (length octets))
         :while (< off2 end))))
  (values string))

;;;; Binary Input

(defmethod stream-read-byte ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of))
      stream
    (flet ((fill-buf-or-eof ()
             (iobuf-reset ib)
             (when (eq :eof (%fill-ibuf read-fn fd ib))
               (return-from stream-read-byte :eof))))
      (when (zerop (iobuf-length ib))
        (fill-buf-or-eof))
      (iobuf-pop-octet ib))))

;;;; Binary Output

(defmethod stream-write-byte ((stream dual-channel-gray-stream) integer)
  (check-type integer ub8 "an unsigned 8-bit value")
  (with-accessors ((ob output-buffer-of))
      stream
    (%flush-obuf-if-needed stream)
    (iobuf-push-octet ob integer)))
