(in-package :incudine-tests)

(deftest gen-polynomial.1
    (cffi:with-foreign-object (arr 'sample 64)
      (funcall (gen:polynomial '(10 -37 5) :xmin 1 :xmax 100) arr 64)
      (loop for i below 64 collect (sample->fixnum (smp-ref arr i))))
  (5 -29 -14 48 158 317 523 776 1078 1428 1825 2270 2763 3304 3893 4530
   5214 5947 6727 7555 8431 9355 10327 11346 12414 13529 14692 15903 17162
   18468 19823 21225 22676 24174 25720 27313 28955 30645 32382 34167 36000
   37881 39810 41787 43811 45884 48004 50172 52388 54652 56963 59323 61730
   64185 66689 69239 71838 74485 77179 79922 82712 85550 88436 91370))

(deftest gen-polynomial.2
    (with-cleanup
      (mapcar 'sample->fixnum
        (buffer->list
          (make-buffer 64
            :fill-function (gen:polynomial
                             '(5 14 -3) :xmin 1 :xmax 100 :normalize-p nil)))))
  (-3 30 88 169 275 404 557 734 935 1160 1409 1682 1979 2300 2645 3013
   3406 3822 4263 4727 5215 5727 6264 6824 7408 8015 8647 9303 9983 10686
   11414 12165 12941 13740 14563 15411 16282 17177 18096 19039 20005 20996
   22011 23049 24112 25198 26309 27443 28601 29783 30990 32220 33474 34751
   36053 37379 38729 40102 41500 42921 44367 45836 47329 48846))

(deftest gen-polynomial.3
    (with-buffer (buf 64)
      (fill-buffer buf (gen:polynomial
                         '(1 -5 19) :xmin 1 :xmax 100 :normalize-p nil))
      (loop for i below 64
            collect (sample->fixnum (smp-ref (buffer-data buf) i))))
  (19 13 13 17 26 40 58 82 110 143 180 223 270 322 379 441 507 579 655 735
   821 911 1006 1106 1211 1321 1435 1554 1678 1807 1940 2078 2221 2369 2522
   2679 2841 3008 3180 3356 3538 3724 3915 4110 4311 4516 4726 4941 5160 5385
   5614 5848 6087 6330 6578 6831 7089 7352 7619 7892 8169 8450 8737 9028))

(deftest gen-polynomial.4
    (with-buffer (buf 16 :fill-function (gen:polynomial '(-1 1 0 0) :xmin 0))
      (loop for i below 16
            collect (sample->fixnum (* 1000 (buffer-value buf i)))))
  (0 24 92 193 317 454 595 728 846 937 991 1000 952 838 647 371))
