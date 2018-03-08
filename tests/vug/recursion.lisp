(in-package :incudine-tests)

(dsp! dsp-recursive-aux ()
  (initialize (error "IGNORE THIS ERROR")))

(dsp! dsp-recursive-test-1 ((x fixnum) (acc list))
  (initialize
    (push x (cdr acc))
    (when (< x 10)
      (dsp-recursive-test-1 (the fixnum (1+ x)) acc))
    (when (and (> x 5) (oddp x))
      (dsp-recursive-aux))))

(deftest dsp-recursion.1
    (let ((acc (list nil)))
      (bounce-to-buffer (*buffer-test-c1*)
        (dsp-recursive-test-1 0 acc))
      (values
        (every 'null-node-p
               (incudine::int-hash-table-items incudine::*nrt-node-hash*))
        (cdr acc)))
  T
  (10 9 8 7 6 5 4 3 2 1 0))
