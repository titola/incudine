(in-package :incudine-tests)

(defun gen-partials-test-1 (lst)
  (cffi:with-foreign-object (arr 'sample 64)
    (funcall (gen:partials lst) arr 64)
    (loop for i below 64 collect (sample->fixnum (smp-ref arr i)))))

(deftest gen-partials.1
    (gen-partials-test-1 '(100))
  (0 9 19 29 38 47 55 63 70 77 83 88 92 95 98 99 100 99 98 95 92 88 83
   77 70 63 55 47 38 29 19 9 0 -10 -20 -30 -39 -48 -56 -64 -71 -78 -84
   -89 -93 -96 -99 -100 -100 -100 -99 -96 -93 -89 -84 -78 -71 -64 -56
   -48 -39 -30 -20 -10))

(deftest gen-partials.2
    (gen-partials-test-1 '(100 0 50 0 25 0 12 0 6 0 3 0 1))
  (0 51 88 105 106 101 96 91 85 79 75 72 70 69 67 67 67 67 67 69 70 72
   75 79 85 91 96 101 106 105 88 51 0 -52 -89 -106 -107 -102 -97 -92 -86
   -80 -76 -73 -71 -70 -68 -68 -67 -68 -68 -70 -71 -73 -76 -80 -86 -92 -97
   -102 -107 -106 -89 -52))

(deftest gen-partials.3
    (gen-partials-test-1 '((1 100) (3 50) (5 25) (7 12) (9 6) (11 3) (13 1)))
  (0 51 88 105 106 101 96 91 85 79 75 72 70 69 67 67 67 67 67 69 70 72
   75 79 85 91 96 101 106 105 88 51 0 -52 -89 -106 -107 -102 -97 -92 -86
   -80 -76 -73 -71 -70 -68 -68 -67 -68 -68 -70 -71 -73 -76 -80 -86 -92 -97
   -102 -107 -106 -89 -52))

(deftest gen-partials.4
    (gen-partials-test-1 '((1 50 .75 50)))
  (0 0 0 2 3 5 8 11 14 18 22 26 30 35 40 45 49 54 59 64 69 73 77 81 85 88
   91 94 96 97 99 99 100 99 99 97 96 94 91 88 85 81 77 73 69 64 59 54 50
   45 40 35 30 26 22 18 14 11 8 5 3 2 0 0))

(deftest gen-partials.5
    (let ((buf (make-buffer 64 :fill-function (gen:partials '((1 50 .75 50))
                                                            :normalize-p nil))))
      (loop for i below 64 collect (sample->fixnum (smp-ref (data buf) i))))
  (0 0 0 2 3 5 8 11 14 18 22 26 30 35 40 45 49 54 59 64 69 73 77 81 85 88
   91 94 96 97 99 99 100 99 99 97 96 94 91 88 85 81 77 73 69 64 59 54 50
   45 40 35 30 26 22 18 14 11 8 5 3 2 0 0))

(deftest gen-partials.6
    (let ((buf (make-buffer 64)))
      (setf (data buf) (gen:partials '(100) :normalize-p nil))
      (loop for i below 64 collect (sample->fixnum (smp-ref (data buf) i))))
  (0 9 19 29 38 47 55 63 70 77 83 88 92 95 98 99 100 99 98 95 92 88 83
   77 70 63 55 47 38 29 19 9 0 -10 -20 -30 -39 -48 -56 -64 -71 -78 -84
   -89 -93 -96 -99 -100 -100 -100 -99 -96 -93 -89 -84 -78 -71 -64 -56
   -48 -39 -30 -20 -10))
