(in-package :incudine-tests)

(defun gen-fir-from-env-1 (order srate env)
  (with-cleanup
    (let ((b (make-buffer order
               :fill-function (gen:fir env :sample-rate srate))))
      (mapcar (lambda (x) (floor (* x 1000))) (buffer->list b)))))

(defun gen-fir-from-buffer-1 (order vals)
  (with-cleanup
    (let ((b (make-buffer order :initial-contents vals)))
      (fill-buffer b (gen:fir nil))
      (mapcar (lambda (x) (floor (* x 1000))) (buffer->list b)))))

(defun gen-fir-hilbert-1 (buffer-size &key window-function normalize-p)
  (mapcar
    (lambda (x) (truncate (* x 100000)))
    (with-cleanup
      (buffer->list
        (make-buffer buffer-size
          :fill-function (gen:hilbert :window-function window-function)
          :normalize-p normalize-p)))))

(deftest gen-fir.1
    (equal (gen-fir-from-env-1 12 48000 '(999 0 1000 1 2000 1 2001 0))
           (gen-fir-from-buffer-1 12 '(0 0 1 1 0 0 0 0 0 0 0 0)))
  T)

(deftest gen-fir.2
    (equal (gen-fir-from-env-1 15 1 '(0 0 .1 1.00001 .2 .5 .3 .8 .4 0))
           (gen-fir-from-buffer-1 15 '(0 1/3 2/3 1 5/6 2/3 .5 .6 .7 .8 .4 0 0 0 0)))
  T)

(deftest gen-hilbert.1
    (gen-fir-hilbert-1 75)
  (-137 0 -157 0 -205 0 -284 0 -399 0 -556 0 -760 0 -1019 0 -1343 0 -1743 0
   -2241 0 -2863 0 -3658 0 -4708 0 -6168 0 -8377 0 -12212 0 -20905 0 -63556
   0
   63556 0 20905 0 12212 0 8377 0 6168 0 4708 0 3658 0 2863 0 2241 0 1743 0
   1343 0 1019 0 760 0 556 0 399 0 284 0 205 0 157 0 137))

(deftest gen-hilbert.2
    (gen-fir-hilbert-1 76)
  (-137 0 -157 0 -205 0 -284 0 -399 0 -556 0 -760 0 -1019 0 -1343 0 -1743 0
   -2241 0 -2863 0 -3658 0 -4708 0 -6168 0 -8377 0 -12212 0 -20905 0 -63556
   0
   63556 0 20905 0 12212 0 8377 0 6168 0 4708 0 3658 0 2863 0 2241 0 1743 0
   1343 0 1019 0 760 0 556 0 399 0 284 0 205 0 157 0 137
   0))

(deftest gen-hilbert.3
    (gen-fir-hilbert-1 75 :window-function #'rectangular-window)
  (-1720 0 -1818 0 -1929 0 -2053 0 -2195 0 -2357 0 -2546 0 -2767 0 -3031 0 -3350 0
   -3744 0 -4244 0 -4897 0 -5787 0 -7073 0 -9094 0 -12732 0 -21220 0 -63661
   0
   63661 0 21220 0 12732 0 9094 0 7073 0 5787 0 4897 0 4244 0 3744 0 3350 0
   3031 0 2767 0 2546 0 2357 0 2195 0 2053 0 1929 0 1818 0 1720))

(deftest gen-hilbert.4
    (gen-fir-hilbert-1 75 :window-function (gen:kaiser 6))
  (-25 0 -59 0 -111 0 -186 0 -291 0 -433 0 -618 0 -858 0 -1164 0 -1550 0 -2038 0
   -2658 0 -3458 0 -4521 0 -6002 0 -8239 0 -12109 0 -20841 0 -63534
   0
   63534 0 20841 0 12109 0 8239 0 6002 0 4521 0 3458 0 2658 0 2038 0 1550 0
   1164 0 858 0 618 0 433 0 291 0 186 0 111 0 59 0 25))

(deftest gen-hilbert.5
    (gen-fir-hilbert-1 75 :normalize-p t)
  (-216 0 -247 0 -322 0 -447 0 -628 0 -875 0 -1197 0 -1604 0 -2113 0 -2743 0
   -3526 0 -4505 0 -5756 0 -7408 0 -9706 0 -13180 0 -19215 0 -32893 0 -99999
   0
   99999 0 32893 0 19215 0 13180 0 9706 0 7408 0 5756 0 4505 0 3526 0 2743 0
   2113 0 1604 0 1197 0 875 0 628 0 447 0 322 0 247 0 216))
