(in-package :cl-user)

(defpackage :incudine-tests
  (:use :cl :incudine :incudine.util :incudine.vug :incudine.analysis
        #+sbcl :sb-rt #-sbcl :rtest)
  (:import-from #+sbcl :sb-rt #-sbcl :rtest
                #:*compile-tests* #:*expected-failures*)
  (:import-from :alexandria #:define-constant #:format-symbol #:with-gensyms)
  (:import-from #+sbcl :sb-md5 #-sbcl :md5 #:md5sum-sequence)
  (:export #:run-tests))

(in-package :incudine-tests)

(defvar *test-hook* nil)
(declaim (type list *test-hook*))

(defun run-tests (&key ((:compiled *compile-tests*)))
  (let ((rt-p (eq (rt-status) :started)))
    (if rt-p (rt-stop) (init))
    (incudine::call-hooks 'test *test-hook*)
    (multiple-value-prog1 (do-tests)
      (when rt-p (rt-start)))))

(defun two-decimals (x)
  (/ (round (* x 100.0)) 100.0))
