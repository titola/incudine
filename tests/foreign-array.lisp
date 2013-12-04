(in-package :incudine-tests)

(deftest foreign-array.1
    (let* ((pool-size (get-rt-memory-free-size))
           (*rt-thread* (bt:current-thread))
           (arr (vug::make-foreign-array 8 'sample :zero-p t))
           (test1 (= pool-size (get-rt-memory-free-size))))
      (free arr)
      (values test1 (= pool-size (get-rt-memory-free-size))))
  NIL T)

(deftest foreign-array.2
    (let ((arr (make-foreign-array 8 'sample :zero-p t))
          (x0 (coerce 0 'sample))
          (x1 (coerce 0.12345 'sample)))
      (setf (smp-ref (data arr) 1) x1)
      (let ((y0 (smp-ref (data arr) 0))
            (y1 (smp-ref (data arr) 1)))
        (free arr)
        (values (= y0 x0) (= y1 x1))))
  T T)
