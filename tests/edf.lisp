(in-package :incudine-tests)

(deftest edf.1
    (progn
      (flush-pending)
      (incudine.edf::heap-empty-p))
  T)

(deftest edf.2
    (let ((incudine::*sample-counter* incudine::*nrt-sample-counter*)
          (acc nil)
          (last 0.0)
          (count 0))
      (loop for i below 32
            for time in '(844 186 655 896 453 76 621 38 603 582 450
                          717 508 238 366 50 564 395 18 446 935 345
                          642 816 176 706 868 356 201 473 457 154)
            do (incudine.edf::%at (coerce time 'sample)
                                  (lambda (time value)
                                    (push (cons time value) acc))
                                  `(,time ,i)))
      (setf last (incudine.edf:last-time))
      (setf count (incudine.edf:heap-count))
      (incudine::reset-sample-counter)
      (loop while (< (now) 1000) do
           (incudine.edf::sched-loop)
           (incudine::incf-sample-counter))
      (incudine::reset-sample-counter)
      (values (mapcar (lambda (x) (cons (truncate (car x)) (cdr x)))
                      (nreverse acc))
              last count))
  ((18 . 18) (38 . 7) (50 . 15) (76 . 5) (154 . 31) (176 . 24) (186 . 1)
   (201 . 28) (238 . 13) (345 . 21) (356 . 27) (366 . 14) (395 . 17) (446 . 19)
   (450 . 10) (453 . 4) (457 . 30) (473 . 29) (508 . 12) (564 . 16) (582 . 9)
   (603 . 8) (621 . 6) (642 . 22) (655 . 2) (706 . 25) (717 . 11) (816 . 23)
   (844 . 0) (868 . 26) (896 . 3) (935 . 20))
  #.(sample 935)
  32)
