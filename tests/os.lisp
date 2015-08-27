;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.os test suite.

(in-package :iolib-tests)

(in-suite :iolib.os)

#+unix
(test (run-program/pipes-do-not-block :compile-at :definition-time)
  ;; Try to output a lot of data (more than the pipe buffer size) to stdout and stderr
  ;; and see how RUN-PROGRAM deals with it.
  (let* ((data-file "/bin/sh")
         (data-file-length (with-open-file (stream data-file)
                             (file-length stream)))
         (command (concatenate 'string "/bin/cat " data-file)))
    (flet
        ((run-program* (&rest args)
           (format *error-output* "~&;; Calling ~S on ~S~%" 'iolib.os:run-program args)
           (iolib.os:run-program args :stdin :pipe :stdout :pipe :external-format :iso-8859-1)))
      (is (< (expt 2 16) data-file-length)
          "The ~S test is useless if the data file is smaller than the pipe buffer size."
          'run-program/pipes-do-not-block)
      (finishes
        (multiple-value-bind (return-code stdout stderr)
            (run-program* "/bin/sh" "-c" command)
          (is (= 0 return-code))
          (is (= data-file-length (length stdout)))
          (is (= 0 (length stderr)))))
      (finishes
        ;; As of this writing the following call will hang. The reason is that RUN-PROGRAM
        ;; tries to fully read the stdout pipe without simultaneously reading stderr, but
        ;; this invocation only writes to stderr and thus eventually overfills the stderr pipe.
        (multiple-value-bind (return-code stdout stderr)
            (run-program* "/bin/sh" "-c" (concatenate 'string command " 1>&2"))
          (is (= 0 return-code))
          (is (= 0 (length stdout)))
          (is (= data-file-length (length stderr))))))))
