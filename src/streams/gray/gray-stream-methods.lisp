;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Implementation using Gray streams.
;;;

(in-package :iolib.streams)

;;;-------------------------------------------------------------------------
;;; Instance Initialization
;;;-------------------------------------------------------------------------

(defun free-stream-buffers (ibuf obuf)
  (when ibuf (free-iobuf ibuf))
  (when obuf (free-iobuf obuf)))

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
  (with-accessors ((ibuf input-buffer-of)
                   (obuf output-buffer-of)
                   (ef external-format-of))
      stream
    (setf ibuf (allocate-iobuf input-buffer-size)
          obuf (allocate-iobuf output-buffer-size)
          ef external-format)
    (trivial-garbage:finalize stream (lambda () (free-stream-buffers ibuf obuf)))))


;;;-------------------------------------------------------------------------
;;; Common Methods
;;;-------------------------------------------------------------------------

(defmethod stream-element-type ((stream dual-channel-gray-stream))
  '(unsigned-byte 8))

;; TODO: use the buffer pool
(defmethod close :around ((stream dual-channel-gray-stream) &key abort)
  (with-accessors ((ibuf input-buffer-of)
                   (obuf output-buffer-of))
      stream
    (trivial-garbage:cancel-finalization stream)
    (unless (or abort (null ibuf)) (finish-output stream))
    (free-stream-buffers ibuf obuf)
    (setf ibuf nil obuf nil))
  (call-next-method)
  stream)

(defmethod close ((stream dual-channel-gray-stream) &key abort)
  (declare (ignore stream abort)))

(defmethod (setf external-format-of)
    (external-format (stream dual-channel-gray-stream))
  (let ((canonical-ef (babel:ensure-external-format external-format)))
    (setf (slot-value stream 'external-format) canonical-ef)
    (setf (values (slot-value stream 'eol-writer)
                  (slot-value stream 'eol-finder))
          (case (babel:external-format-eol-style canonical-ef)
            (:lf   (values #'stream-write-lf   #'stream-find-lf))
            (:crlf (values #'stream-write-crlf #'stream-find-crlf))
            (:cr   (values #'stream-write-cr   #'stream-find-cr))))))


;;;-------------------------------------------------------------------------
;;; Input Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-input ((stream dual-channel-gray-stream))
  (iobuf-reset (input-buffer-of stream)))

(declaim (inline %read-sequence))
(defun %read-sequence (stream seq start end)
  (check-bounds seq start end)
  (when (< start end)
    (etypecase seq
      (ub8-sarray (%read-into-simple-array-ub8 stream seq start end))
      (string     (%read-into-string stream seq start end))
      (ub8-vector (%read-into-vector stream seq start end)))))

(declaim (inline read-sequence*))
(defun read-sequence* (stream sequence &key (start 0) end)
  (%read-sequence stream sequence start end))

(defmethod stream-read-sequence
    ((stream dual-channel-gray-stream) sequence start end &key)
  (%read-sequence stream sequence start end))

(defmethod drain-input-buffer
    ((stream dual-channel-gray-stream) sequence &key (start 0) end)
  (check-bounds sequence start end)
  (with-accessors ((ib input-buffer-of))
      stream
    (let ((nbytes (min (- end start)
                       (iobuf-length ib))))
      (when (plusp nbytes)
        (iobuf-copy-into-lisp-array ib (iobuf-start ib)
                                    sequence start
                                    nbytes)
        (incf (iobuf-start ib) nbytes)
        (let ((len (iobuf-length ib)))
          (values (+ start nbytes)
                  (and (plusp len) len)))))))


;;;-------------------------------------------------------------------------
;;; Output Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-output ((stream dual-channel-gray-stream))
  (iobuf-reset (output-buffer-of stream))
  (setf (dirtyp stream) nil))

(defmethod stream-finish-output ((stream dual-channel-gray-stream))
  (with-accessors ((fd output-fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (with-hangup-guard stream
      (%write-octets-from-iobuf write-fn fd ob))
    (setf dirtyp nil)))

(defmethod stream-force-output ((stream dual-channel-gray-stream))
  (setf (dirtyp stream) t))

(declaim (inline %write-sequence))
(defun %write-sequence (stream seq start end)
  (check-bounds seq start end)
  (when (< start end)
    (etypecase seq
      (ub8-sarray (%write-simple-array-ub8 stream seq start end))
      (string     (stream-write-string stream seq start end))
      (ub8-vector (%write-vector-ub8 stream seq start end))
      (vector     (%write-vector stream seq start end)))))

(declaim (inline write-sequence*))
(defun write-sequence* (stream sequence &key (start 0) end)
  (%write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream dual-channel-gray-stream)
                                  sequence start end &key)
  (%write-sequence stream sequence start end))


;;;-------------------------------------------------------------------------
;;; Character Input
;;;-------------------------------------------------------------------------

(defun %stream-rewind-iobuf (stream iobuf encoding)
  (maybe-rewind-iobuf iobuf encoding)
  (setf (unread-index-of stream) (iobuf-start iobuf)))

(defmethod stream-read-char ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (ib input-buffer-of)
                   (read-fn read-fn-of)
                   (unread-index unread-index-of)
                   (ef external-format-of))
      stream
    (let ((encoding (babel:external-format-encoding ef)))
      (%stream-rewind-iobuf stream ib encoding)
      (let ((eofp (eql :eof (%fill-ibuf ib fd read-fn))))
        (if (and eofp (iobuf-empty-p ib))
            :eof
            ;; At this point, there's at least one octet in the buffer
            (let ((line-end (funcall (eol-finder-of stream) ib fd read-fn nil eofp)))
              (if (eql #\Newline line-end)
                  #\Newline
                  (decode-one-char fd read-fn ib encoding))))))))

(defmethod stream-read-char-no-hang ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of)
                   (ef external-format-of))
      stream
    (let ((encoding (babel:external-format-encoding ef)))
      (%stream-rewind-iobuf stream ib encoding)
      (let ((eofp (eql :eof (%fill-ibuf/no-hang ib fd read-fn))))
        (if (iobuf-empty-p ib)
            (if eofp :eof nil)
            ;; At this point, there's at least one octet in the buffer
            (let ((line-end (funcall (eol-finder-of stream) ib fd read-fn t eofp)))
              (cond (;; There's a CR but it's not EOF so we could still receive a LF
                     (and (eql :incomplete line-end) (not eofp))
                     nil)
                    ((eql #\Newline line-end)
                     #\Newline)
                    (t
                     (decode-one-char/no-hang ib encoding)))))))))

(defun %stream-unread-char (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (unread-index unread-index-of))
      stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index)
         (setf start unread-index))
        ((= start unread-index)
         (error 'no-characters-to-unread :stream stream))
        (t (bug "On stream ~S the buffer start(~A) is less than the unread index(~A)."
                stream start unread-index)))))
  nil)

(defmethod stream-unread-char ((stream dual-channel-gray-stream) character)
  (declare (ignore character))
  (%stream-unread-char stream))

(defmethod stream-peek-char ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char stream)))
    (cond ((eql :eof char)
           :eof)
          (t
           (%stream-unread-char stream)
           char))))

;; (defmethod stream-read-line ((stream dual-channel-gray-stream))
;;   )

(defmethod stream-listen ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((characterp char) (stream-unread-char stream char) t)
          ((eql :eof char) nil)
          (t t))))


;;;-------------------------------------------------------------------------
;;; Character Output
;;;-------------------------------------------------------------------------

(defmethod stream-write-char ((stream dual-channel-gray-stream)
                              (character character))
  (%flush-obuf-if-needed stream)
  (if (char= character #\Newline)
      (funcall (eol-writer-of stream) stream)
      (let ((string (make-string 1 :initial-element character)))
        (declare (dynamic-extent string))
        (stream-write-string stream string))))

(defmethod stream-line-column ((stream dual-channel-gray-stream))
  0)

(defmethod stream-start-line-p ((stream dual-channel-gray-stream))
  nil)

(defmethod stream-terpri ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) nil)

(defmethod stream-fresh-line ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) t)

(defmethod stream-write-string ((stream dual-channel-gray-stream)
                                (string string) &optional (start 0) end)
  (check-bounds string start end)
  (do* ((ef (external-format-of stream))
        (encoding (babel:external-format-encoding ef)))
      ((= start end))
    (case (char string start)
      (#\Newline
       (funcall (eol-writer-of stream) stream)
       (incf start))
      (t
       (setf start (%write-string-chunk stream string start end encoding)))))
  string)


;;;-------------------------------------------------------------------------
;;; Binary Input
;;;-------------------------------------------------------------------------

(defmethod stream-read-byte ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of))
      stream
    (flet ((fill-buf-or-eof ()
             (iobuf-reset ib)
             (when (eql :eof (%fill-ibuf ib fd read-fn))
               (return* :eof))))
      (when (zerop (iobuf-length ib))
        (fill-buf-or-eof))
      (iobuf-pop-octet ib))))


;;;-------------------------------------------------------------------------
;;; Binary Output
;;;-------------------------------------------------------------------------

(defmethod stream-write-byte ((stream dual-channel-gray-stream) integer)
  (check-type integer ub8 "an unsigned 8-bit value")
  (with-accessors ((ob output-buffer-of))
      stream
    (with-hangup-guard stream
      (%flush-obuf-if-needed stream))
    (iobuf-push-octet ob integer)))


;;;-------------------------------------------------------------------------
;;; Buffer-related functions
;;;-------------------------------------------------------------------------

(defmethod input-buffer-size ((stream dual-channel-gray-stream))
  (iobuf-length (input-buffer-of stream)))

(defmethod input-buffer-empty-p ((stream dual-channel-gray-stream))
  (iobuf-empty-p (input-buffer-of stream)))

(defmethod output-buffer-size ((stream dual-channel-gray-stream))
  (iobuf-length (output-buffer-of stream)))

(defmethod output-buffer-empty-p ((stream dual-channel-gray-stream))
  (iobuf-empty-p (output-buffer-of stream)))
