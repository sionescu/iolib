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

(in-package :net.sockets)

(iolib-utils:define-constant +max-octets-per-char+ 6)

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
  (with-slots ((ib input-buffer) (ob output-buffer)
               (ef external-format)) s
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

(defmethod stream-element-type ((stream active-socket))
  :default)

;; TODO: use abort
;; TODO: use the buffer pool
(defmethod close :around ((stream active-socket) &key abort)
  (declare (ignore abort))
  (with-slots ((ib input-buffer)
               (ob output-buffer)) stream
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

(defmethod stream-clear-input ((stream active-socket))
  (with-slots ((ib input-buffer)) stream
    (iobuf-reset ib)
    nil))

;; (defmethod stream-read-sequence ((stream active-socket) seq
;;                                  &optional start end)
;;   )

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; Output Methods ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defmethod stream-clear-output ((stream active-socket))
  (with-slots ((ob output-buffer)) stream
    (iobuf-reset ob)
    nil))

(defmethod stream-finish-output ((stream active-socket))
  (with-slots ((ob output-buffer) fd) stream
    (flush-obuf ob fd)
    nil))

(defmethod stream-force-output ((stream active-socket))
  ;; FIXME: add non-blocking version of this?
  ;; and/or re-write the flush code in a non-blocking variant,
  ;; and have the finish-output synchronize on the result.
  (stream-finish-output stream))

;; (defmethod stream-read-sequence ((stream active-socket) seq
;;                                  &optional start end)
;;   )

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; Character Input ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

(defun fill-ibuf (buf fd &optional timeout)
  (when timeout
    (let ((status
           (iomux:wait-until-fd-ready fd :read timeout)))
      (unless (member :read status)
        ;; FIXME signal something better
        (return-from fill-ibuf :timeout))))
  (let ((num (et:read fd (cffi:inc-pointer (iobuf-data buf)
                                           (iobuf-start buf))
                      (- (iobuf-size buf)
                         (iobuf-end buf)))))
    (if (zerop num)
        :eof
        (incf (iobuf-end buf) num))))

(defun maybe-find-line-ending (fd ib ef)
  (let* ((start-off (iobuf-start ib))
         (char-code (bref ib start-off)))
    (block nil
      (ecase (ioenc:ef-line-terminator ef)
        (:unix (when (= char-code (char-code #\Linefeed))
                 (incf (iobuf-start ib))
                 (return (values #\Newline 1))))
        (:mac (when (= char-code (char-code #\Return))
                (incf (iobuf-start ib))
                (return (values #\Newline 1))))
        (:dos (when (= char-code (char-code #\Return))
                (when (and (= (iobuf-length ib) 1)
                           (eq (fill-ibuf ib fd) :eof))
                  (incf (iobuf-start ib))
                  (return (values #\Return 1)))
                (when (= (bref ib (1+ start-off))
                         (char-code #\Linefeed))
                  (incf (iobuf-start ib) 2)
                  (return (values #\Newline 2)))))))))

(defmethod stream-read-char ((stream active-socket))
  (with-slots ((fd fd) (ib input-buffer)
               (unread-index ibuf-unread-index)
               (pos istream-pos)
               (ef external-format)) stream
    (setf unread-index (iobuf-start ib))
    (let ((str (make-string 1))
          (ret nil))
      (flet ((fill-buf-or-eof ()
               ;; FIXME - what if we can't refill, in the middle of a wide-char??
               (setf ret (fill-ibuf ib fd))
               (when (eq ret :eof)
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
        (multiple-value-bind (line-end bytes-consumed)
            (maybe-find-line-ending fd ib ef)
          (when line-end
            (incf pos bytes-consumed)
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
           (incf pos ret)
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
                 (return (values #\Newline 1))))
        (:mac (when (= char-code (char-code #\Return))
                (incf (iobuf-start ib))
                (return (values #\Newline 1))))
        (:dos (when (= char-code (char-code #\Return))
                (when (= (iobuf-length ib) 1)
                  (incf (iobuf-start ib))
                  (return :starvation))
                (when (= (bref ib (1+ start-off))
                         (char-code #\Linefeed))
                  (incf (iobuf-start ib) 2)
                  (return (values #\Newline 2)))))))))

(defmethod stream-read-char-no-hang ((stream active-socket))
  (with-slots ((fd fd) (ib input-buffer)
               (pos istream-pos)
               (ef external-format)) stream
    (let ((str (make-string 1))
          (ret nil)
          (eof nil))
      (block nil
        (when (< 0 (iobuf-end-space-length ib) 4)
          (iobuf-copy-data-to-start ib))
        (when (and (iomux:fd-ready-p fd :read)
                   (eql :eof (fill-ibuf ib fd)))
          (setf eof t))
        (when (zerop (iobuf-length ib))
          (return (if eof :eof nil)))
        ;; line-end handling
        (multiple-value-bind (line-end bytes-consumed)
            (maybe-find-line-ending-no-hang fd ib ef)
          (cond ((eql line-end :starvation)
                 (if eof
                     (progn
                       (incf pos)
                       (return #\Return))
                     (return nil)))
                ((characterp line-end)
                 (incf pos bytes-consumed)
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
        (incf pos ret)
        (incf (iobuf-start ib) ret)
        (char str 0)))))

(defun %stream-unread-char (stream)
  ;; unreading anything but the latest character is wrong,
  ;; but checking is not mandated by the standard
  #+super-anal-checks
  (progn
    (%stream-unread-char stream)
    (unless (ignore-errors (eql (stream-read-char stream) character))
      (error "Trying to unread wrong character ~S" character)))
  (declare (type active-socket stream))
  (with-slots ((ib input-buffer) (unread-index ibuf-unread-index)) stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index)
         (setf start unread-index))
        (t
         (error "No uncommitted character to unread")))))
  nil)

(defmethod stream-unread-char ((stream active-socket) character)
  (declare (ignore character))
  (%stream-unread-char stream))

(defmethod stream-peek-char ((stream active-socket))
  (let ((char (stream-read-char stream)))
    (cond ((eq char :eof) :eof)
          (t (%stream-unread-char stream)
             (values char)))))

;; (defmethod stream-read-line ((stream active-socket))
;;   (with-slots ((fd fd) (ib input-buffer)
;;                (pos istream-pos)
;;                (ef external-format)) stream
;;     (let ((str (make-string 80)) (strsz 80) (strlen 0)
;;           (chars-out 0) (bytes-in 0)
;;           (ret nil))
;;       )))

(defmethod stream-listen ((stream active-socket))
  (characterp (stream-read-char-no-hang stream)))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Character Output ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun buffer-string-to-octets (string buffer start end ef fd &optional max-char-num)
  (declare (string string)
           (type iobuf buffer)
           (type buffer-index start)
           (type buffer-index end)
           (ignore fd)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (unless max-char-num (setf max-char-num -1))
  (let ((ptr start) oldptr
        (pos -1) oldpos
        (char-count -1))
    (labels
        ((input ()
           (prog1 (char string ptr) (incf ptr)))
         (output (octet)
           (setf (bref buffer (incf pos)) octet))
         (error-fn (symbol)
           (restart-case
               (error symbol :string string
                      :start start :end end
                      :position oldptr
                      :external-format (ef-name ef))
             (use-value (s)
               :report "Supply a replacement character."
               :interactive ioenc::read-replacement-char
               s)
             (use-standard-unicode-replacement ()
               :report "Use standard UCS replacement character"
               (code-char ioenc::+replacement-char+))
             (stop-decoding ()
               :report "Stop decoding and return to last good offset."
               (setf pos oldpos)
               (exit))))
         (exit ()
           (return-from buffer-string-to-octets (1+ pos))))
      (loop :while (and (< ptr end)
                        (/= (incf char-count) max-char-num))
         :do (setf oldpos pos oldptr ptr)
         (ioenc::char-to-octets ef #'input #'output #'error-fn (- end ptr)))
      (exit))))

(defun flush-obuf (buf fd &optional timeout)
  ;; FIXME - ought to loop partial writes until actual timeout,
  ;; interleaving write 
  ;; computing the initial deadline, and retrying until it's passed
  (flet ((write-once ()
           (let* ((num (et:write
                        fd
                        (cffi:inc-pointer (iobuf-data buf)
                                          (iobuf-start buf))
                        (iobuf-length buf))))
             (if (zerop num)
                 nil
                 (progn (incf (iobuf-start buf) num) t))))
         (emptyp ()
           (when (iobuf-empty-p buf)
             (iobuf-reset buf)
             t)))
    (if (emptyp)
        (values t nil)
        (if timeout
            (loop :with deadline := (+ (iomux::gettime) timeout)
               :for status := (iomux:wait-until-fd-ready fd :write timeout) :do
               (unless (member :write status)
                 ;; FIXME signal something better -- maybe analyze the status
                 (return (values nil :timeout)))
               (unless (write-once)
                 (return (values nil :fail)))
               (when (emptyp)
                 (return (values t nil)))
               (setf timeout (- deadline (iomux::gettime)))
               (unless (> timeout 0)
                 (return (values nil :timeout))))
            (loop :for status := (iomux:wait-until-fd-ready fd :write nil) :do
               (unless (member :write status)
                 ;; FIXME signal something better -- maybe analyze the status
                 (return (values nil :fail)))
               (unless (write-once)
                 (return (values nil :fail)))
               (when (emptyp)
                 (return (values t nil))))))))


(defmethod %stream-write-octets ((stream active-socket) octets
                                 &optional start end)
  ;; FIXME: when calling write-sequence with a simple-array of octets
  ;; do required I/O directly, not through a buffer
  (check-type octets (simple-array ub8 (*)))
  (let ((max (length octets)))
    (if start
        (check-type start unsigned-byte)
        (setf start 0))
    (if end
        (progn
          (check-type end unsigned-byte)
          (assert (<= end max)))
        (setf end max)))
  (with-slots ((buf output-buffer) fd) stream
    (loop :while (< start end) :do
         (let ((len (min (- end start) (iobuf-end-space-length buf))))
           (setf *print-readably* nil)
           ;; FIXME: optimize this BLT
           (loop :for i :from start
              :for j :from (iobuf-end buf)
              :repeat len :do
              (setf (bref buf j) (aref octets i)))
           (incf (iobuf-end buf) len)
           (incf start len)
           (when (= (iobuf-end buf) (iobuf-size buf))
             (or (flush-obuf buf fd)
                 ;; FIXME: better error handling
                 (error "Failed to write octets")))))))

(defmethod stream-write-char ((stream active-socket) character)
  ;; FIXME: avoid consing a string here. At worst, declare it dynamic-extent
  (stream-write-string stream (make-string 1 :initial-element character)))

;; (defmethod stream-advance-to-column ((stream active-socket)
;;                                      (column integer)))

;; (defmethod stream-line-column ((stream active-socket)))

;; (defmethod stream-line-length ((stream active-socket)))

(defmethod stream-start-line-p ((stream active-socket))
  nil)

;; (defmethod stream-terpri ((stream active-socket)))

;; (defmethod stream-fresh-line ((stream active-socket)))

(defmethod stream-write-string ((stream active-socket)
                                (string string)
                                &optional start end)
  ;; FIXME: have the ef do i/o directly into the existing buffer,
  ;; don't do double buffering of I/O
  (%stream-write-octets
   stream
   (ioenc:string-to-octets string :start start :end end
                           :external-format (slot-value stream 'external-format))))

;; FIXME: isn't there a generic stream-write-sequence???


;;;;;;;;;;;;;;;;;;
;;              ;;
;; Binary Input ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defmethod stream-read-byte ((stream active-socket))
  (with-slots ((fd fd) (ib input-buffer)
               (pos istream-pos)) stream
    (flet ((fill-buf-or-eof ()
             (when (eq :eof (fill-ibuf ib fd))
               (return-from stream-read-byte :eof))))
      (when (zerop (iobuf-length ib))
        (iobuf-reset ib)
        (fill-buf-or-eof))
      (prog1 (bref ib (iobuf-start ib))
        (incf pos)
        (incf (iobuf-start ib))))))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; Binary Output ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

;; (defmethod stream-write-byte ((stream active-socket) (integer integer))
;;   )
