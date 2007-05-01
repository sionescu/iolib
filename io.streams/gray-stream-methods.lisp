;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :io.streams)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Instance Initialization ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: use the buffer pool
;; TODO: handle instance reinitialization
(defmethod shared-initialize :after ((s dual-channel-gray-stream) slot-names
                                     &key (input-buffer-size +bytes-per-iobuf+)
                                     (output-buffer-size +bytes-per-iobuf+)
                                     (external-format :default))
  (declare (ignore slot-names))
  (check-type input-buffer-size buffer-index)
  (check-type output-buffer-size buffer-index)
  (when (open-stream-p s) (close s))
  (with-accessors ((ib input-buffer-of) (ob output-buffer-of)
                   (ef external-format-of)) s
    (setf ib (allocate-iobuf input-buffer-size)
          ob (allocate-iobuf output-buffer-size))
    (setf ef (etypecase external-format
               (symbol (find-external-format external-format))
               ((and list (not null))
                (apply #'make-external-format external-format))))))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; Common Methods ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defmethod stream-element-type ((stream dual-channel-gray-stream))
  '(unsigned-byte 8))

;; TODO: use the buffer pool
(defmethod close :around ((stream dual-channel-gray-stream) &key abort)
  (with-accessors ((ib input-buffer-of)
                   (ob output-buffer-of)) stream
    (unless (or abort (null ib)) (finish-output stream))
    (when ib (free-iobuf ib))
    (when ob (free-iobuf ob))
    (setf ib nil ob nil))
  (call-next-method)
  (values stream))

(defmethod close ((stream dual-channel-gray-stream) &key abort)
  (declare (ignore stream abort)))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; Input Methods ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defmethod stream-clear-input ((stream dual-channel-gray-stream))
  (with-accessors ((ib input-buffer-of)) stream
    (iobuf-reset ib)
    nil))

(defun %fill-ibuf (buf fd &optional timeout)
  (when timeout
    (let ((status
           (iomux:wait-until-fd-ready fd :read timeout)))
      ;; FIXME signal something better
      (cond ((member :timeout status)
             (return-from %fill-ibuf :timeout))
            ((member :error status)
             (error "WAIT-UNTIL-FD-READY returned :ERROR on FD ~S" fd)))))
  (let ((num (et:repeat-upon-eintr
               (et:read fd (iobuf-end-pointer buf)
                        (iobuf-end-space-length buf)))))
    (if (zerop num)
        :eof
        (incf (iobuf-end buf) num))))

(defun %read-into-simple-array-ub8 (stream array start end)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (fd input-fd-of)) stream
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
         :when (eql :eof (%fill-ibuf ib fd)) :do (loop-finish)
         :finally (return array-offset)))))

(defun %read-into-string (stream string start end)
  (declare (type dual-channel-gray-stream stream))
  (loop :for offset :from start :below end
        :for char := (stream-read-char stream)
     :if (eql char :eof) :do (loop-finish)
     :else :do (setf (char string offset) char)
     :finally (return offset)))

(defun %read-into-vector (stream vector start end)
  (declare (type dual-channel-gray-stream stream))
  (loop :for offset :from start :below end
        :for octet := (stream-read-byte stream)
     :if (eql octet :eof) :do (loop-finish)
     :else :do (setf (aref vector offset) octet)
     :finally (return offset)))

#-clisp
(defmethod #-openmcl stream-read-sequence
           #+openmcl stream-read-vector
           ((stream dual-channel-gray-stream) seq
            &optional (start 0) end)
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (ub8-sarray
       (%read-into-simple-array-ub8 stream seq start end))
      (string
       (%read-into-string stream seq start end))
      (vector
       (%read-into-vector stream seq start end)))))

#+clisp
(defmethod stream-read-byte-sequence ((stream dual-channel-gray-stream) seq
                                      &optional (start 0) end
                                      no-hang interactive)
  (declare (ignore no-hang interactive))
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (ub8-sarray
       (%read-into-simple-array-ub8 stream seq start end))
      (vector
       (%read-into-vector stream seq start end)))))

#+clisp
(defmethod stream-read-char-sequence ((stream dual-channel-gray-stream) seq
                                      &optional (start 0) end)
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (string
       (%read-into-string stream seq start end)))))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; Output Methods ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defun %write-n-bytes (buf fd nbytes &optional timeout)
  (let ((bytes-written 0))
    (flet ((write-once ()
             (let ((num (handler-case
                            (et:repeat-upon-eintr
                              (et:write fd (inc-pointer buf bytes-written)
                                        nbytes))
                          (et:epipe (err) (declare (ignore err))
                            (return-from %write-n-bytes (values nil :eof))))))
               (unless (zerop num) (incf bytes-written num))))
           (buffer-emptyp () (zerop nbytes)))
      (let (num)
        (if (buffer-emptyp) (values t nil)
            (et:repeat-decreasing-timeout (timeout-var timeout)
              (unless (setf num (write-once))
                (when (member :error (iomux:wait-until-fd-ready fd :write))
                  ;; FIXME signal something better -- maybe analyze the status
                  (return-from %write-n-bytes (values nil :fail))))
              (when (buffer-emptyp) (return-from %write-n-bytes (values t bytes-written)))
              (when (zerop timeout-var) (return-from %write-n-bytes (values nil :timeout)))))))))

(defun %flush-obuf (buf fd &optional timeout)
  (let ((bytes-written 0))
    (flet ((write-once ()
             (let ((num (handler-case
                            (et:repeat-upon-eintr
                              (et:write fd (iobuf-start-pointer buf)
                                        (iobuf-length buf)))
                          (et:epipe (err) (declare (ignore err))
                            (return-from %flush-obuf (values nil :eof))))))
               (unless (zerop num)
                 (incf (iobuf-start buf) num)
                 (incf bytes-written num))))
           (buffer-emptyp ()
             (when (iobuf-empty-p buf)
               (iobuf-reset buf) t)))
      (let (num)
        (if (buffer-emptyp) (values t nil)
            (et:repeat-decreasing-timeout (timeout-var timeout)
              (unless (setf num (write-once))
                (when (member :error (iomux:wait-until-fd-ready fd :write))
                  ;; FIXME signal something better -- maybe analyze the status
                  (return-from %flush-obuf (values nil :fail))))
              (when (buffer-emptyp) (return-from %flush-obuf (values t bytes-written)))
              (when (zerop timeout-var) (return-from %flush-obuf (values nil :timeout)))))))))

;; TODO: add timeout support
(defun %flush-obuf-if-needed (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((fd output-fd-of) (ob output-buffer-of)
                   (must-flush-output-p must-flush-output-p)) stream
    (when (or must-flush-output-p (iobuf-full-p ob))
      (%flush-obuf ob fd)
      (setf must-flush-output-p nil))))

(defmethod stream-clear-output ((stream dual-channel-gray-stream))
  (with-accessors ((ob output-buffer-of)
                   (must-flush-output-p must-flush-output-p)
                   (fd output-fd-of)) stream
    (iobuf-reset ob)
    (setf must-flush-output-p nil)
    nil))

(defmethod stream-finish-output ((stream dual-channel-gray-stream))
  (with-accessors ((ob output-buffer-of)
                   (must-flush-output-p must-flush-output-p)
                   (fd output-fd-of)) stream
    (%flush-obuf ob fd)
    (setf must-flush-output-p nil)
    nil))

(defmethod stream-force-output ((stream dual-channel-gray-stream))
  (setf (must-flush-output-p stream) t))

(defun %write-simple-array-ub8 (stream array start end)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ob output-buffer-of)
                   (fd output-fd-of)) stream
    (let ((octets-needed (- end start)))
      (if (<= octets-needed (iobuf-end-space-length ob))
          (progn
            (iobuf-copy-from-lisp-array array start ob
                                        (iobuf-end ob) octets-needed)
            (incf (iobuf-end ob) octets-needed)
            (%flush-obuf-if-needed stream))
          (with-pointer-to-vector-data (ptr array)
            (%flush-obuf ob fd)
            (let ((ret (%write-n-bytes (inc-pointer ptr start)
                                       fd octets-needed)))
              (when (numberp ret)
                (incf (iobuf-end ob) octets-needed)))))
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

#-clisp
(defmethod #-openmcl stream-write-sequence
           #+openmcl stream-write-vector
           ((stream dual-channel-gray-stream) seq
            &optional (start 0) end)
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (ub8-sarray
       (%write-simple-array-ub8 stream seq start end))
      (string
       (stream-write-string stream seq start end))
      ((vector ub8)
       (%write-vector-ub8 stream seq start end))
      (vector
       (%write-vector stream seq start end)))))

#+clisp
(defmethod stream-write-byte-sequence ((stream dual-channel-gray-stream) seq
                                       &optional (start 0) end
                                       no-hang interactive)
  (declare (ignore no-hang interactive))
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (ub8-sarray
       (%write-simple-array-ub8 stream seq start end))
      ((vector ub8)
       (%write-vector-ub8 stream seq start end))
      (vector
       (%write-vector stream seq start end)))))

#+clisp
(defmethod stream-write-char-sequence ((stream dual-channel-gray-stream) seq
                                       &optional (start 0) end)
  (setf (values start end) (%check-bounds seq start end))
  (when (< start end)
    (etypecase seq
      (string
       (stream-write-string stream seq start end)))))

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; Character Input ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

(defun maybe-find-line-ending (fd ib ef)
  (let* ((start-off (iobuf-start ib))
         (char-code (bref ib start-off)))
    (block nil
      (ecase (ioenc:ef-line-terminator ef)
        (:unix (when (= char-code (char-code #\Linefeed))
                 (incf (iobuf-start ib))
                 (return #\Newline)))
        (:mac (when (= char-code (char-code #\Return))
                (incf (iobuf-start ib))
                (return #\Newline)))
        (:dos (when (= char-code (char-code #\Return))
                (when (and (= (iobuf-length ib) 1)
                           (eql (%fill-ibuf ib fd) :eof))
                  (incf (iobuf-start ib))
                  (return #\Return))
                (when (= (bref ib (1+ start-off))
                         (char-code #\Linefeed))
                  (incf (iobuf-start ib) 2)
                  (return #\Newline))))))))

(define-constant +max-octets-per-char+ 6)

;; FIXME: currently we return :EOF when read(2) returns 0
;;        we should distinguish hard end-of-files(EOF and buffer empty)
;;        from soft end-of-files(EOF and *some* bytes still in the buffer
;;        but not enough to make a full character)
(defmethod stream-read-char ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of) (ib input-buffer-of)
                   (unread-index ibuf-unread-index-of)
                   (ef external-format-of)) stream
    (setf unread-index (iobuf-start ib))
    (let ((str (make-string 1))
          (ret nil))
      (flet ((fill-buf-or-eof ()
               (setf ret (%fill-ibuf ib fd))
               (when (eql ret :eof)
                 (return-from stream-read-char :eof))))
        (cond ((zerop (iobuf-length ib))
               (iobuf-reset ib)
               (fill-buf-or-eof))
              ;; Some encodings such as CESU or Java's modified UTF-8 take
              ;; as much as 6 bytes per character. Make sure we have enough
              ;; space to collect read-ahead bytes if required.
              ((< 0 (iobuf-end-space-length ib) +max-octets-per-char+)
               (iobuf-copy-data-to-start ib)
               (setf unread-index 0)))
        ;; line-end handling
        (let ((line-end
               (maybe-find-line-ending fd ib ef)))
          (when line-end
            (return-from stream-read-char line-end)))
        (tagbody :start
           (handler-case
               (setf ret (nth-value 1 (ioenc::%octets-to-string
                                       (iobuf-data ib) str
                                       (iobuf-start ib)
                                       (iobuf-end ib) ef 1)))
             (end-of-input-in-character (err)
               (declare (ignore err))
               (fill-buf-or-eof)
               (go :start)))
           (incf (iobuf-start ib) ret))
        (char str 0)))))

(defun maybe-find-line-ending-no-hang (fd ib ef)
  (declare (ignore fd))
  (let* ((start-off (iobuf-start ib))
         (char-code (bref ib start-off)))
    (block nil
      (ecase (ioenc:ef-line-terminator ef)
        (:unix (when (= char-code (char-code #\Linefeed))
                 (incf (iobuf-start ib))
                 (return #\Newline)))
        (:mac (when (= char-code (char-code #\Return))
                (incf (iobuf-start ib))
                (return #\Newline)))
        (:dos (when (= char-code (char-code #\Return))
                (when (= (iobuf-length ib) 1)
                  (incf (iobuf-start ib))
                  (return :starvation))
                (when (= (bref ib (1+ start-off))
                         (char-code #\Linefeed))
                  (incf (iobuf-start ib) 2)
                  (return #\Newline))))))))

(defmethod stream-read-char-no-hang ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of) (ib input-buffer-of)
                   (ef external-format-of)) stream
    (let ((str (make-string 1))
          (ret nil)
          (eof nil))
      (block nil
        (when (< 0 (iobuf-end-space-length ib) 4)
          (iobuf-copy-data-to-start ib))
        (when (and (iomux:fd-ready-p fd :read)
                   (eql :eof (%fill-ibuf ib fd)))
          (setf eof t))
        (when (zerop (iobuf-length ib))
          (return (if eof :eof nil)))
        ;; line-end handling
        (let ((line-end
               (maybe-find-line-ending-no-hang fd ib ef)))
          (cond ((eql line-end :starvation)
                 (return (if eof #\Return nil)))
                ((characterp line-end)
                 (return line-end))))
        ;; octet decoding
        (handler-case
            (setf ret (nth-value 1 (ioenc::%octets-to-string
                                    (iobuf-data ib) str
                                    (iobuf-start ib)
                                    (iobuf-end ib) ef 1)))
          (end-of-input-in-character (err)
            (declare (ignore err))
            (return nil)))
        (incf (iobuf-start ib) ret)
        (char str 0)))))

(defun %stream-unread-char (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (unread-index ibuf-unread-index-of)) stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index)
         (setf start unread-index))
        (t
         (error "No uncommitted character to unread")))))
  nil)

(defmethod stream-unread-char ((stream dual-channel-gray-stream) character)
  ;; unreading anything but the latest character is wrong,
  ;; but checking is not mandated by the standard
  #+iolib-debug
  (progn
    (%stream-unread-char stream)
    (unless (ignore-errors (eql (stream-read-char stream) character))
      (error "Trying to unread wrong character ~S" character)))
  #-iolib-debug
  (declare (ignore character))
  #-iolib-debug
  (%stream-unread-char stream))

(defmethod stream-peek-char ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char stream)))
    (cond ((eql char :eof) :eof)
          (t (%stream-unread-char stream)
             (values char)))))

;; (defmethod stream-read-line ((stream dual-channel-gray-stream))
;;   )

(defmethod stream-listen ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((characterp char)
           (stream-unread-char stream char)
           t)
          ((eql char :eof)
           nil)
          (t t))))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Character Output ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defmethod stream-write-char ((stream dual-channel-gray-stream)
                              (character character))
  (%flush-obuf-if-needed stream)
  (if (eql character #\Newline)
      (%write-line-terminator stream (ioenc:ef-line-terminator (external-format-of stream)))
      ;; FIXME: avoid consing a string here. At worst, declare it dynamic-extent
      (stream-write-string stream (make-string 1 :initial-element character))))

(defmethod stream-start-line-p ((stream dual-channel-gray-stream))
  (values nil))

(defmethod stream-terpri ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) nil)

(defmethod stream-fresh-line ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) t)

(define-constant +unix-line-terminator+
    (make-array 1 :element-type 'ub8 :initial-contents '(10)))
(define-constant +dos-line-terminator+
    (make-array 2 :element-type 'ub8 :initial-contents '(13 10)))
(define-constant +mac-line-terminator+
    (make-array 1 :element-type 'ub8 :initial-contents '(13)))

(defun %write-line-terminator (stream line-terminator)
  (case line-terminator
    (:unix (%write-simple-array-ub8 stream +unix-line-terminator+ 0 1))
    (:dos  (%write-simple-array-ub8 stream +dos-line-terminator+  0 2))
    (:mac  (%write-simple-array-ub8 stream +mac-line-terminator+  0 1))))

(defmethod stream-write-string ((stream dual-channel-gray-stream)
                                (string string)
                                &optional (start 0) end)
  (setf (values start end) (%check-bounds string start end))
  (when (< start end)
    (let* ((octets nil)
           (ef (external-format-of stream))
           (line-terminator (ioenc:ef-line-terminator ef)))
      (loop :for off1 := start :then (1+ off2)
            :for nl-off := (position #\Newline string :start off1)
            :for off2 := (or nl-off end)
         :when nl-off :do (%write-line-terminator stream line-terminator)
         :when (> off2 off1) :do
         (setf octets (ioenc:string-to-octets
                       string :start off1 :end off2
                       :external-format ef))
         (%write-simple-array-ub8 stream octets 0 (length octets))
         :while (< off2 end))))
  (values string))

;;;;;;;;;;;;;;;;;;
;;              ;;
;; Binary Input ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defmethod stream-read-byte ((stream dual-channel-gray-stream))
  (with-accessors ((fd input-fd-of)
                   (ib input-buffer-of)) stream
    (flet ((fill-buf-or-eof ()
             (iobuf-reset ib)
             (when (eql :eof (%fill-ibuf ib fd))
               (return-from stream-read-byte :eof))))
      (when (zerop (iobuf-length ib))
        (fill-buf-or-eof))
      (iobuf-pop-octet ib))))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; Binary Output ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defmethod stream-write-byte ((stream dual-channel-gray-stream) integer)
  (check-type integer ub8 "an unsigned 8-bit value")
  (with-accessors ((ob output-buffer-of)) stream
    (%flush-obuf-if-needed stream)
    (iobuf-push-octet ob integer)))
