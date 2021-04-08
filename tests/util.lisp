(in-package :incudine-tests)

;;;; Misc

(deftest linear->db
    (mapcar (lambda (x) (floor (linear->db x)))
                     '(0.00001 0.0001 0.001 0.01 .1 .15 .2 .25 .33
                       .5 .68 .772 .9 1 1.9 3.4))
  (-100 -81 -61 -41 -21 -17 -14 -13 -10 -7 -4 -3 -1 0 5 10))

(deftest db->linear
    (mapcar (lambda (x) (two-decimals (db->linear x)))
            '(-100 -10 -24 -12 -6 -3 0 6 12))
  (0.0 0.32 0.06 0.25 0.5 0.71 1.0 2.0 3.98))

(deftest t60->pole
    (let ((*sample-duration* (/ (sample 48000))))
      (values (truncate (* (t60->pole 0.001) 10000))))
  8659)

(deftest next-power-of-two
    (mapcar #'next-power-of-two '(5 10 21 44 1234 54321 3456789))
  (8 16 32 64 2048 65536 4194304))

(deftest power-of-two-p
    (mapcar #'power-of-two-p '(7 8 12345 2048 4194304 65535 65536 512 128 8193))
  (NIL T NIL T T NIL T T T NIL))

(deftest sample->fixnum.1
    (mapcar #'sample->fixnum
            (mapcar (lambda (x) (sample x))
                    '(1.234567 5.4358 7.66854 -15.738 5632.589 299.32563
                      -4984.593 38.4892 -93.4583)))
  (1 5 7 -16 5632 299 -4985 38 -94))

(deftest sample->fixnum.2
    (mapcar (lambda (x) (sample->fixnum x :roundp t))
            (mapcar (lambda (x) (sample x))
                    '(1.234567 5.4358 7.66854 -15.738 5632.589 299.32563
                      -4984.593 38.4892 -93.4583)))
  (1 5 8 -16 5633 299 -4985 38 -93))

(deftest float->fixnum.1
    (mapcar #'float->fixnum
            '(1.234567 5.4358 7.66854 -15.738 5632.589 299.32563
              -4984.593 38.4892 -93.4583))
  (1 5 7 -16 5632 299 -4985 38 -94))

(deftest float->fixnum.2
    (mapcar (lambda (x) (float->fixnum x :roundp t))
            '(1.234567 5.4358 7.66854 -15.738 5632.589 299.32563
              -4984.593 38.4892 -93.4583))
  (1 5 8 -16 5633 299 -4985 38 -93))

(deftest calc-lobits
    (loop for i = +table-maxlen+ then (ash i -1)
          while (> i 0)
          collect (incudine::calc-lobits i))
  #.(loop for i to +max-lobits+ collect i))

(deftest parse-float.1
    (let ((str "test 123 345.67 -8910.1112    1314.15"))
      (flet ((parse-test (start &optional end)
               (multiple-value-list
                 (parse-float str :start start :end end))))
        (values (parse-test 5)
                (parse-test 8)
                (parse-test 15)
                (parse-test 26)
                (parse-test 10 14)
                (parse-test 0))))
  (123.0 8) (345.67 15) (-8910.111 26) (1314.15 37) (45.6 14) (NIL 0))

(deftest dochannels
    (let ((acc))
      (dochannels (ch 8 (nreverse acc))
        (push ch acc)))
  (0 1 2 3 4 5 6 7))

(define-constant +rt-block-size+ 64)

(defvar *rt-block-size* 1)

(deftest set-rt-block-size
    (let ((incudine::*default-rt-loop-cb* #'incudine::rt-loop-1)
          (incudine::*block-input-samples* *number-of-input-bus-channels*)
          (incudine::*block-output-samples* *number-of-output-bus-channels*)
          (incudine::*block-size* 1)
          (new-block-size 32)
          (*rt-block-size* 1)
          (tests nil))
      (macrolet ((add-test (block-size cb)
                   `(progn
                      (set-rt-block-size ,block-size)
                      (push (list (= (block-size) ,block-size)
                                  (eq incudine::*default-rt-loop-cb* ,cb))
                            tests)))
                 (test-result ()
                   `(nreverse tests)))
        (add-test new-block-size #'incudine::rt-loop-1)
        (add-test *rt-block-size* #'incudine::rt-loop-1)
        (setf *rt-block-size* 128)
        (add-test *rt-block-size* #'incudine::rt-loop-1)
        (add-test +rt-block-size+ #'incudine::rt-loop-64)
        (add-test 1 #'incudine::rt-loop-1)
        (add-test 64 #'incudine::rt-loop-64)
        (test-result)))
  ((T NIL) (T T) (T NIL) (T T) (T T) (T T)))

;;;; DEFUN*, LAMBDA* and DEFMACRO*

(defun* hi (a (b 32) (c "hi")) (list a b c))
(defun* star0 ((a 0) (b (+ a 4)) (c (+ a 7))) (list a b c))
(defun* star1 (&rest a &rest b) (mapcar #'+ a b))
(defun* star2 ((b 3) &rest x (c 1)) (list b c x))
(defun* star3 ((a :uno) (b :due) (c :tre)) (list a b c))

(defmacro* add-2 (a (b 2)) `(+ ,a ,b))
(defmacro* star4 ((a 2) b &rest c (d 44) . e) `(list ,a ,b ',c ,d ',e))

(defmacro* with-star-test-1 ((&optional-key (a 1) (b 2)) &rest rest)
  `(list ,a ,b ,@rest))

(defmacro* with-star-test-2 ((&optional-key (a 1) (b 2))
                             (&optional-key (&optional-key (c 3) (d 4)) (e 5))
                             (f 6) g . h)
  `(list ,a ,b ,c ,d ,e ,f ,g ,@h))

(deftest lambda-star.1
    (list (hi 1) (hi :b 2 :a 3) (hi 3 2 1)
          (star0 :b 2 :a 60)
          (star1 1 2 3 4 5)
          (star2 32) (star2 1 2 3 4 5)
          (star3 :tre) (star3 :tre :due :uno) (star3 :due :c :uno :b :tre)
          (add-2 1 3) (add-2 1) (add-2 :b 3 :a 1)
          (star4 1 2 3 4 5 6 7 8)
          (with-star-test-1)
          (with-star-test-1 ())
          (with-star-test-1 (:b 200) 300 400)
          (with-star-test-1 (10 20) 30 40)
          (with-star-test-1 () 10 20 30)
          (with-star-test-2)
          (with-star-test-2 () ((:d 123)))
          (with-star-test-2 () () :h (1 2 3))
          (with-star-test-2 (10) ((20) 30) :g 40 :f 50 :h (60)))
  ((1 32 "hi") (3 2 "hi") (3 2 1)
   (60 2 67)
   (3 5 7 9)
   (32 1 NIL) (1 3 (2 3 4 5))
   (:TRE :DUE :TRE) (:TRE :DUE :UNO) (:DUE :TRE :UNO)
   4 3 4
   (1 2 (3 4 5 6 7 8) 4 (5 6 7 8))
   (1 2)
   (1 2)
   (1 200 300 400)
   (10 20 30 40)
   (1 2 10 20 30)
   (1 2 3 4 5 6 NIL)
   (1 2 3 123 5 6 NIL)
   (1 2 3 4 5 6 NIL 1 2 3)
   (10 2 20 4 30 50 40 60)))

(deftest lambda-star.2
    (list (funcall (lambda* ((b 3) &rest x (c 1)) (list b c x)) 32)
          (funcall (lambda* ((b 3) &rest x (c 1)) (list b c x)) 1 2 3 4 5)
          (funcall (lambda* ((b 3) &rest x (c 1) . d) (list b c x d)) 1 2 3 4 5))
  ((32 1 NIL)
   (1 3 (2 3 4 5))
   (1 3 (2 3 4 5) (4 5))))

;;;; Interpolation

(deftest linear-interp
    (let ((y0 (sample 3.0d0))
          (y1 (sample 11.0d0)))
      (mapcar (lambda (x) (two-decimals (linear-interp x y0 y1)))
              (mapcar (lambda (x) (sample x))
                      '(0.0d0 .1d0 .231d0 .5d0 .75d0 1d0))))
  (3.0 3.8 4.85 7.0 9.0 11.0))

(deftest cos-interp
    (let ((y0 (sample 3.0d0))
          (y1 (sample 11.0d0)))
      (mapcar (lambda (x) (two-decimals (cos-interp x y0 y1)))
              (mapcar (lambda (x) (sample x))
                      '(0.0d0 .11d0 .33d0 .5d0 .75d0 1d0))))
  (3.0 3.24 4.96 7.0 9.83 11.0))

(deftest cubic-interp
    (let ((y0 (sample 3.0d0))
          (y1 (sample 11.0d0))
          (y2 (sample 16.32d0))
          (y3 (sample 29.5d0)))
      (mapcar (lambda (x) (two-decimals (cubic-interp x y0 y1 y2 y3)))
              (mapcar (lambda (x) (sample x))
                      '(0.0d0 .12d0 .33d0 .5d0 .75d0 1d0))))
  (11.0 11.71 12.67 13.34 14.5 16.32))

;;;; Random

(deftest seed-random-state.1
    (flet ((rand-gen1 ()
             (loop repeat 16 collect (random 1000)))
           (rand-gen2 ()
             (loop repeat 16 collect
                  (incudine.gen::ran-flat (incudine.external::gsl-random-generator)
                                          0.0d0 1000.0d0))))
      (seed-random-state 12345)
      (let ((l1 (rand-gen1))
            (l2 (rand-gen2)))
        (seed-random-state 54321)
        (let ((l3 (rand-gen1))
              (l4 (rand-gen2)))
          (seed-random-state 12345)
          (values (equal l1 (rand-gen1)) (equal l1 l3)
                  (equal l2 (rand-gen2)) (equal l2 l4)))))
  T NIL T NIL)

(deftest seed-random-state.2
    (flet ((rand-gen1 ()
             (loop repeat 16 collect (random 1000)))
           (rand-gen2 ()
             (loop repeat 16 collect
                  (incudine.gen::ran-flat (incudine.external::gsl-random-generator)
                                          0.0d0 1000.0d0))))
      (let* ((*random-state* (make-random-state))
             (state1 (make-random-state nil))
             (state2 (make-random-state nil)))
        (seed-random-state state1)
        (let ((l1 (rand-gen1))
              (l2 (rand-gen2)))
          (seed-random-state (make-random-state t))
          (let ((l3 (rand-gen1))
                (l4 (rand-gen2)))
            (seed-random-state state2)
            (values (equal l1 (rand-gen1)) (equal l1 l3)
                    (equal l2 (rand-gen2)) (equal l2 l4))))))
  T NIL T NIL)

;;;; Macros

(deftest with-samples.1
    (with-samples ((a 3) (b 4))
      (+ a b))
  #.(sample 7))

(deftest with-samples.2
    (with-samples* ((a 3)
                    (b (+ a 4)))
      (* a b))
  #.(sample 21))

(deftest with-samples.3
    (let ((a 1) (b 2) (c 3))
      (with-samples ((a 10) (b (* a 2)) c d (e (+ b 3)))
        (+ a b c d e)))
  #.(sample 17))

(deftest with-samples.4
    (let* ((a 1) (b 2) (c 3))
      (with-samples* ((a 10) (b (* a 2)) c d (e (+ b 3)))
        (+ a b c d e)))
  #.(sample 53))

;; Trivial case: (incudine:free nil)
(deftest with-cleanup-empty
    (with-cleanup)
  nil)

(deftest with-cleanup.1
    (flet ((free-objects ()
             (incudine.util::incudine-object-pool-size incudine::*buffer-pool*)))
    (let* ((r0 (free-objects))
           (r1 0)
           (allocated (min 123 r0)))
      (assert (plusp r0))
      (with-cleanup
        (let ((lst (loop repeat allocated collect (make-buffer 128))))
          (setf r1 (free-objects))))
      (values (= (free-objects) r0)
              (= (- r0 r1) allocated))))
  T T)

(deftest lambda-list-to-star-list.1
    (mapcar #'lambda-list-to-star-list
      '((x &key y &aux z)
        (&rest incudine.util::optional-keywords &aux (#:lambda-list '(x y z)))
        (&rest incudine.util::optional-keywords &aux (lambda-list '(x y z)))
        (x &key y &aux (z 123) (lambda-list '(1 2 3)))))
  (NIL (INCUDINE.UTIL::&ANY X Y Z) NIL NIL))
