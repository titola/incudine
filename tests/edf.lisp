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

(deftest edf.3
    (progn
      (flush-pending)
      (incudine.edf:add-flush-pending-hook '*)
      (incudine.edf:add-flush-pending-hook '*)
      (incudine.edf:add-flush-pending-hook '*)
      (flush-pending)
      (let* ((res1 incudine.edf::*flush-pending-hook*)
             (count 0)
             (inc-count (lambda () (incf count))))
        (flet ((add-hook ()
                 (incudine.edf:add-flush-pending-hook inc-count)
                 (incudine.edf:add-flush-pending-hook '*)
                 (incudine.edf:add-flush-pending-hook inc-count)
                 (incudine.edf:add-flush-pending-hook '*)))
          (add-hook)
          (flush-pending)
          (let ((res2 (cons incudine.edf::*flush-pending-hook* count)))
            (setf count 0)
            (add-hook)
            (incudine.edf:remove-flush-pending-hook inc-count)
            (flush-pending)
            (values res1 res2 incudine.edf::*flush-pending-hook* count)))))
  NIL (NIL . 2) NIL 0)

(deftest edf-sort.1
    (with-local-time ()
      (let ((stack nil)
            (len (1- incudine.edf::*heap-size*)))
        (loop for i below len
              ;; Correct order of the events with the same time.
              for time = 0 then (if (> (random 100) 50) (1+ time) time)
              do (at time (lambda (x) (push x stack)) i))
        (loop until (incudine.edf:heap-empty-p) do
                (incudine.edf::sched-loop)
                (incf (now)))
        (equal (alexandria:iota len) (reverse stack))))
  T)

(defun edf-unschedule-test (arg acc)
  (push arg (car acc)))

(deftest edf-unschedule-if.1
    (with-local-time ()
      (let ((acc (list nil)))
        (dotimes (i 50)
          (at i (if (oddp i)
                    #'edf-unschedule-test
                    (lambda (arg acc) (push arg (car acc))))
              i acc))
        (unschedule-if (lambda (time function args)
                         (declare (ignore time args))
                         (eq function #'edf-unschedule-test)))
        (loop until (incudine.edf:heap-empty-p) do
                (incudine.edf::sched-loop)
                (incf (now)))
        (nreverse (car acc))))
  (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48))

(deftest edf-unschedule-if.2
    (with-local-time ()
      (let ((acc (list nil)))
        (dotimes (i 50)
          (at i #'edf-unschedule-test i acc))
        (unschedule-if (lambda (time function args)
                         (declare (ignore function args))
                         (< 20 time 40)))
        (loop until (incudine.edf:heap-empty-p) do
                (incudine.edf::sched-loop)
                (incf (now)))
        (nreverse (car acc))))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
   40 41 42 43 44 45 46 47 48 49))
