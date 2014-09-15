(in-package :cl-user)

(defpackage :incudine-tests
  (:use :cl :incudine :incudine.util :incudine.vug #+sbcl :sb-rt #-sbcl :rtest)
  (:shadow #:dsp!)
  (:import-from #+sbcl :sb-rt #-sbcl :rtest
                #:*compile-tests* #:*expected-failures*)
  (:import-from :alexandria #:define-constant #:format-symbol #:with-gensyms)
  (:export #:run-tests))

(in-package :incudine-tests)

(defun run-tests (&key ((:compiled *compile-tests*)))
  (let ((rt-p (eq (rt-status) :started)))
    (if rt-p (rt-stop) (init))
    (multiple-value-prog1 (do-tests)
      (when rt-p (rt-start)))))

(defun two-decimals (x)
  (* .01 (round (* x 100))))
