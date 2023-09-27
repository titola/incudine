(in-package :incudine-tests)

;; Foreign types.
(deftest arg-types.1
    (progn
      (define-ugen arg-test-1 list ((a foreign-float) (b foreign-double)
                                    (c int32) (d int64) (p foreign-pointer))
        (list a b c d (cffi:null-pointer-p p)))
      (let* ((u (funcall (arg-test-1 1 2 3 4 (cffi:null-pointer))))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (1.0f0 2.0d0 3 4 t))

;; Foreign types (alias).
(deftest arg-types.2
    (progn
      (define-ugen arg-test-2 list ((a f32) (b f64) (c i32) (d i64) (p ptr))
        (list a b c d (cffi:null-pointer-p p)))
      (let* ((u (funcall (arg-test-2 1 2 3 4 (cffi:null-pointer))))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (1.0f0 2.0d0 3 4 t))

;; Coercing of the numbers inside WITH.
(deftest init-types.1
    (progn
      (define-ugen arg-test-3 list ()
        (with ((a 1)
               (b 2)
               (c 3)
               (d 4))
          (declare (sample a) (f32 b) (f64 c) (int32 d))
          (list a b c d)))
      (let* ((u (funcall (arg-test-3)))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (1.0d0 2.0f0 3.0d0 4))

;; Foreign pointer test.
(deftest pointer-test.1
    (progn
      (define-ugen pointer-test-1 boolean ()
        (with ((a 0) (b 0) (c 0) (d 0) (e 0) (f (cffi:null-pointer)))
          (declare (sample a) (i32 b) (i64 c) (f32 d) (f64 e) (ptr f))
          (macrolet ((ptr-test ()
                       `(list ,@(mapcar (lambda (var)
                                          `(cffi:pointerp (get-pointer ,var)))
                                        '(a b c d e f)))))
            (notany #'null (ptr-test)))))
      (let* ((u (funcall (pointer-test-1)))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  t)

(deftest pointer-test.2
    (progn
      (define-ugen pointer-test-2 boolean
          ((a sample) (b i32) (c i64) (d f32) (e f64) (f ptr))
          (macrolet ((ptr-test ()
                       `(list ,@(mapcar (lambda (var)
                                          `(cffi:pointerp (get-pointer ,var)))
                                        '(a b c d e f)))))
            (notany #'null (ptr-test))))
      (let* ((u (funcall (pointer-test-2 1 2 3 4 5 (cffi:null-pointer))))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  t)

(deftest pointer-test.3
    (progn
      (define-ugen pointer-test-3 list ()
        (with ((a (make-frame 8 :initial-element 0.12345d0))
               (b (make-f32-array 8))
               (c (make-f64-array 8))
               (d (make-i32-array 8))
               (e (make-i64-array 8))
               (f (cffi:null-pointer))
               (g 0)
               (l 0)
               (m 0)
               (n 0)
               (p 0))
          (declare (pointer a b c d e f)
                   (sample g) (i32 l) (i64 m) (f32 n) (f64 p))
          (dotimes (i 8)
            (setf (f32-ref b i) (coerce (smp-ref a i) 'single-float))
            (setf (f64-ref c i) (coerce (* 0.123 (f32-ref b i)) 'double-float))
            (setf (i32-ref d i) (floor (* 1000 (f64-ref c i))))
            (setf (i64-ref e i) (ash (i32-ref d i) 3))
            (setf f (cffi:mem-aptr a 'sample i))
            (incf g (+ (smp-ref a i) (smp-ref f 0)))
            (incf l (i32-ref d i))
            (incf m (i64-ref e i))
            (incf n (f32-ref b i))
            (incf p (f64-ref c i)))
          (list (sample->fixnum (* 1d7 g)) l m (floor (* 1e3 n))
                (floor (* 1d7 p)))))
      (let* ((u (funcall (pointer-test-3)))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (19752000 120 960 987 1214748))

(deftest pointer-test.4
    (progn
      (define-ugen pointer-test-4 pointer ()
        (with ((a (make-frame 2))
               (b (make-f32-array 4))
               (c (make-f64-array 6))
               (d (make-i32-array 8))
               (e (make-u32-array 10))
               (f (make-i64-array 12))
               (g (make-u64-array 14))
               (h (make-pointer-array 8)))
          (declare (pointer a b c d e f g h))
          (initialize
           (loop for i from 0
                 for ptr in (list a b c d e f g)
                 do (setf (ptr-ref h i) ptr)))
          h))
      (let ((u (funcall (pointer-test-4))))
        (funcall (ugen-perf-function u))
        (let* ((ptr (ptr-ref (ugen-return-pointer u) 0))
               (res (when (and (eq (foreign-array-type-of ptr) :pointer)
                               (= (foreign-length ptr) 8))
                      (loop for i below 7
                            for vec = (ptr-ref ptr i)
                            collect (foreign-array-type-of vec)
                            collect (foreign-length vec)))))
          (free u)
          res)))
  (SAMPLE 2 :FLOAT 4 :DOUBLE 6 :INT32 8 :UINT32 10 :INT64 12 :UINT64 14))

(deftest pointer-test.5
    (progn
      (define-ugen pointer-test-5 list ()
        (with ((arr (make-i32-array 8 :initial-contents '(0 1 2 3 4 5 6 7))))
          (loop for i below 8 collect (i32-ref arr i))))
      (with-ugen-instance (u pointer-test-5)
        (funcall (ugen-perf-function u))))
  (0 1 2 3 4 5 6 7))
