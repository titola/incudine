(in-package :incudine-tests)

;;;; Misc

(deftest lin->db
    (mapcar (lambda (x) (floor (lin->db x)))
                     '(0.00001 0.0001 0.001 0.01 .1 .15 .2 .25 .33
                       .5 .68 .772 .9 1 1.9 3.4))
  (-100 -81 -61 -41 -21 -17 -14 -13 -10 -7 -4 -3 -1 0 5 10))

(deftest db->lib
    (mapcar (lambda (x) (two-decimals (db->lin x)))
            '(-100 -10 -24 -12 -6 -3 0 6 12))
  (0.0 0.32 0.06 0.25 0.5 0.71 1.0 2.0 3.98))

(deftest t60->pole
    (values (truncate (* (t60->pole 0.001) 10000)))
  8659)

(deftest next-power-of-two
    (mapcar #'next-power-of-two '(5 10 21 44 1234 54321 3456789))
  (8 16 32 64 2048 65536 4194304))

(deftest power-of-two-p
    (mapcar #'power-of-two-p '(7 8 12345 2048 4194304 65535 65536 512 128 8193))
  (NIL T NIL T T NIL T T T NIL))

(deftest sample->fixnum
    (mapcar #'sample->fixnum
            (mapcar (lambda (x) (sample x))
                    '(1.234567 5.4358 7.66854 -15.738 5632.589 299.32563
                      -4984.593 38.4892 -93.4583)))
  (1 5 7 -16 5632 299 -4985 38 -94))

(deftest calc-lobits
    (loop for i = +table-maxlen+ then (ash i -1)
          while (> i 0)
          collect (calc-lobits i))
  #.(loop for i to +max-lobits+ collect i))

(deftest dochannels
    (let ((acc))
      (dochannels (ch 8 (nreverse acc))
        (push ch acc)))
  (0 1 2 3 4 5 6 7))

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

(deftest with-samples
    (with-samples ((a 3) (b 4))
      (+ a b))
  #.(sample 7))

(deftest with-samples*
    (with-samples* ((a 3)
                    (b (+ a 4)))
      (* a b))
  #.(sample 21))
