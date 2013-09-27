(in-package :incudine-tests)

(defun gen-window-test-1 (fn &optional (size 64))
  (cffi:with-foreign-object (arr 'sample size)
    (funcall fn arr size)
    (loop for i below size collect (sample->fixnum (* 1000 (smp-ref arr i))))))

(deftest gen-win-hanning
    (gen-window-test-1 (gen:hanning))
  (0 2 9 21 38 59 84 113 146 182 222 264 308 354 402 450 500 549 597 645
   691 735 777 817 853 886 915 940 961 978 990 997 1000 997 990 978 961
   940 915 886 853 817 777 735 691 645 597 549 499 450 402 354 308 264 222
   182 146 113 84 59 38 21 9 2))

(deftest gen-win-sine
    (gen-window-test-1 (gen:sine-window))
  (0 49 98 146 195 242 290 336 382 427 471 514 555 595 634 671 707 740
   773 803 831 857 881 903 923 941 956 970 980 989 995 998 1000 998 995
   989 980 970 956 941 923 903 881 857 831 803 773 740 707 671 634 595
   555 514 471 427 382 336 290 242 195 146 98 49))
