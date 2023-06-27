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

(defun edf-sched-loop-test ()
  (with-local-time ()
    (loop until (incudine.edf:heap-empty-p) do
         (incudine.edf::sched-loop)
         (incf (now)))))

(deftest edf-sort.1
    (let ((stack nil)
          (len (1- incudine.edf::*heap-size*)))
      (flush-pending)
      (loop for i below len
            ;; Correct order of the events with the same time.
            for time = 0 then (if (> (random 100) 50) (1+ time) time)
            do (at time (lambda (x) (push x stack)) i))
        (edf-sched-loop-test)
        (equal (alexandria:iota len) (reverse stack)))
  T)

;; UNSCHEDULE-IF is slow with too events to unschedule.
(define-constant +max-number-of-events+
  (1- (min 8192 incudine.edf::*heap-size*)))

(defun edf-unschedule-test (arg acc)
  (push arg (car acc)))

(deftest edf-unschedule-if.1
    (let ((acc (list nil)))
      (flush-pending)
      (dolist (i (alexandria:shuffle (alexandria:iota +max-number-of-events+)))
        (at i (if (oddp i)
                  #'edf-unschedule-test
                  (lambda (arg acc) (push arg (car acc))))
            i acc))
      (unschedule-if (lambda (time function args)
                       (declare (ignore time args))
                       (eq function #'edf-unschedule-test)))
      (edf-sched-loop-test)
      (equal (loop for i below +max-number-of-events+ by 2 collect i)
             (nreverse (car acc))))
  t)

(deftest edf-unschedule-if.2
    (let ((acc (list nil))
          (all-times (alexandria:shuffle
                       (alexandria:iota +max-number-of-events+))))
      (flush-pending)
      (dolist (i all-times)
        (at i #'edf-unschedule-test i acc))
      (unschedule-if (lambda (time function args)
                       (declare (ignore function args))
                       (< 20 time 40)))
      (edf-sched-loop-test)
      (sort (set-difference all-times (nreverse (car acc))) #'<))
  (21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39))

(deftest edf-unschedule-if.3
    (let* ((acc (list nil))
           (all-times (alexandria:shuffle
                        (alexandria:iota +max-number-of-events+)))
           (low (random (floor (* +max-number-of-events+ .975))))
           (high (+ low (random (- +max-number-of-events+ low)))))
      (flush-pending)
      (dolist (i all-times)
        (at i #'edf-unschedule-test i acc))
      (unschedule-if (lambda (time function args)
                       (declare (ignore function args))
                       (< low time high)))
      (edf-sched-loop-test)
      (equal (sort (set-difference all-times (nreverse (car acc))) #'<)
             (loop for i from (1+ low) below high collect i)))
  t)

(deftest edf-unschedule-if.4
    (let* ((acc (list nil))
           (len (min 2047 +max-number-of-events+))
           (all-times (alexandria:shuffle (alexandria:iota len)))
           (bad-times (do ((i 0 (1+ i))
                           (times nil)
                           (rep (max 10 (random (ash len -3)))))
                          ((>= i rep) (sort times #'<))
                        (pushnew (random len) times))))
      (flush-pending)
      (dolist (i all-times)
        (at i #'edf-unschedule-test i acc))
      (unschedule-if (lambda (time function args)
                       (declare (ignore function args))
                       (find time bad-times :test #'=)))
      (edf-sched-loop-test)
      (equal (sort (set-difference all-times (nreverse (car acc))) #'<)
             bad-times))
  t)

(deftest resize-nrt-edf-heap.1
    (let ((size *nrt-edf-heap-size*)
          (resize-1 0)
          (resize-2 0)
          (*logger-stream* *null-output*))
      (flet ((resize-test ()
               (list incudine.edf:*heap-size*
                     (length (incudine.edf::heap-data incudine.edf:*heap*)))))
        (let ((*nrt-edf-heap-size* 8192))
          (bounce-to-buffer (*buffer-test-c1* :frames 1)
            (setf resize-1 (resize-test))))
        (bounce-to-buffer (*buffer-test-c1* :frames 1)
          (setf resize-2 (resize-test)))
        (values resize-1
                (equal resize-2 (list size size)))))
  (8192 8192) T)

(deftest edf-next-time.1
    (flet ((times (length offset)
             (loop for i below length
                   collect (+ offset 1d9 (random 1d6))))
           (sched (time-list function)
             (loop for time in time-list
                   for i from 0 do
                   (at time (or function (lambda (x) x)) i))))
      (let* ((len (1- incudine.edf::*heap-size*))
             (n (ash len -1))
             (m (ash n -1))
             (start (now))
             (time-list-1 (times n start))
             (time-list-2 (times (- len n) start)))
        (flush-pending)
        (sched time-list-1 nil)
        (sched time-list-2 #'identity)
        (setf time-list-1 (sort (nconc time-list-1 (copy-list time-list-2)) #'<))
        (setf time-list-2 (sort time-list-2 #'<))
        (prog1 (flet ((test (time-list &optional function start-time-p)
                        (let ((start-time (and start-time-p
                                               (+ (nth m time-list) 1d-1))))
                          (= (incudine.edf:next-time function start-time)
                             (if start-time-p
                                 (or (find-if (lambda (x) (>= x start-time)) time-list)
                                     +sample-zero+)
                                 (first time-list))))))
                 (list (test time-list-1)
                       (test time-list-1 nil t)
                       (test time-list-2 #'identity)
                       (test time-list-2 #'identity t)))
          (flush-pending))))
  (T T T T))
