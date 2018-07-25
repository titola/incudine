(in-package :incudine-tests)

(define-ugen local-time-1 fixnum ()
  (logand (sample->fixnum (now)) #xffff))

(deftest local-time.1
    (with-ugen-instance (u local-time-1)
      (with-local-time ()
        (loop repeat 10
           collect (funcall (ugen-perf-function u))
           do (incf (now)))))
  (0 1 2 3 4 5 6 7 8 9))

(deftest local-time.2
    (with-ugen-instance (u local-time-1)
      (with-local-time (:start 12345)
        (loop repeat 10
           collect (funcall (ugen-perf-function u))
           do (incf (now)))))
  (12345 12346 12347 12348 12349 12350 12351 12352 12353 12354))
