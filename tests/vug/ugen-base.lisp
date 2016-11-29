(in-package :incudine-tests)

(compile-vug 'vug-test-1 'sample)

;;; Test name, return-type, arguments, types and control-flags.
(with-ugen-test (ugen.1)
    (let ((u (ugen 'vug-test-1)))
      (if u
          (values t (incudine.vug::ugen-name u)
                  (incudine.vug::ugen-return-type u)
                  (incudine.vug::ugen-args u)
                  (incudine.vug::ugen-arg-types u)
                  (incudine.vug::ugen-control-flags u))))
  t vug-test-1 sample (freq amp phase) (sample sample sample) (3 1 1))

(compile-vug 'vug-test-2 'sample)

;;; Test arguments and types.
(with-ugen-test (ugen.2)
    (let ((u (ugen 'vug-test-2)))
      (if u
          (values t (incudine.vug::ugen-args u)
                  (incudine.vug::ugen-arg-types u))))
  t (buf rate offset loop-p done-action)
  (buffer sample sample boolean function))

;;; Test RENAME-UGEN and DESTROY-UGEN.
(with-ugen-test (ugen.3)
    (progn
      (define-ugen ugen-tmp sample (amp) (white-noise amp))
      (let ((n1 (incudine.vug::ugen-name (ugen 'ugen-tmp))))
        (rename-ugen 'ugen-tmp 'tmp-ugen)
        (let ((n2 (incudine.vug::ugen-name (ugen 'tmp-ugen))))
          (destroy-ugen 'tmp-ugen)
          (values n1 n2 (null (ugen 'ugen-tmp)) (null (ugen 'tmp-ugen))
                  (null (vug 'ugen-tmp)) (null (vug 'tmp-ugen))))))
  ugen-tmp tmp-ugen t t nil t)

;;; Test FIX-UGEN
(with-ugen-test (ugen.4)
    (flet ((check-cb ()
             (eq (vug::ugen-callback (ugen 'vug-test-1)) #'vug-test-1)))
      (let ((p1 (check-cb)))
        (setf (symbol-function 'vug-test-1) #'*)
        (let ((p2 (check-cb)))
          (fix-ugen 'vug-test-1)
          (values p1 p2 (check-cb)))))
  t nil t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-ugen ugen-test-1 sample (freq amp (nh fixnum))
    (buzz freq amp nh))

  (define-ugen-control-setter ugen-test-1 freq)
  (define-ugen-control-setter ugen-test-1 freq set-ugen-test-1-freq* sample)
  (define-ugen-control-setter ugen-test-1 nh)

  (defstruct (oscil-test-ugen (:include ugen-instance))))

(with-ugen-test (ugen-instance-constructor.1)
    (flet ((value-test (u)
             (dotimes (i 100 (sample->fixnum
                               (* 1e3 (smp-ref (ugen-return-pointer u) 0))))
               (funcall (ugen-perf-function u)))))
      (define-ugen oscil-test sample (freq)
        "UGen Test 1."
        (sine freq 1 0))
      (with-ugen-instance (osc oscil-test 440)
        (let ((doc0 (documentation 'oscil-test 'function))
              (type0 (type-of osc))
              (val0 (value-test osc)))
          (define-ugen oscil-test sample (freq)
            "UGen Test 2."
            (:constructor make-oscil-test-ugen)
            (sine freq 1 0))
          (with-ugen-instance (osc oscil-test 440)
            (let ((doc1 (documentation 'oscil-test 'function))
                  (type1 (type-of osc))
                  (val1 (value-test osc)))
              (destroy-ugen 'oscil-test)
              (destroy-vug 'oscil-test)
              (values doc0 val0 type0 doc1 val1 type1))))))
  "UGen Test 1." -550 ugen-instance
  "UGen Test 2." -550 oscil-test-ugen)

(with-ugen-test (with-ugen-instances.1)
    (with-ugen-instances ((u0 ugen-test-1 50 1e5 10)
                          (u1 ugen-test-1 121 1e5 7)
                          (u2 ugen-test-1 440 1e5 3))
      (let* ((gens (list u0 u1 u2))
             (fns (mapcar #'ugen-perf-function gens)))
        (loop repeat 32
              do (mapc #'funcall fns)
              collect (sample->fixnum
                        (apply #'+ (mapcar (lambda (x)
                                             (smp-ref (ugen-return-pointer x) 0))
                                           gens))))))
  (300000 298894 295597 290169 282710 273352 262264 249642 235705 220692 204854
   188448 171733 154962 138378 122208 106655 91900 78094 65358 53778 43409
   34272 26355 19616 13986 9373 5664 2730 433 -1372 -2830))

(with-ugen-test (ugen.5)
    (let* ((u (funcall (ugen-test-1 100 123456 30)))
           (res (loop repeat 32
                      do (funcall (ugen-perf-function u))
                      collect (sample->fixnum
                                (smp-ref (ugen-return-pointer u) 0)))))
      (free u)
      (values (free-p u) res))
  t
  (123456 120149 110540 95533 76520 55225 33520 13218 -4114 -17310 -25710
   -29204 -28203 -23560 -16439 -8154 -2 6889 11689 13937 13571 10899 6523
   1234 -4114 -8727 -11979 -13490 -13160 -11167 -7919 -3987))

(with-ugen-test (ugen.6)
    (let ((u (funcall (ugen-test-1 100 123456 30))))
      (flet ((vals (freq-setter)
               (loop for i below 32
                     do (funcall (ugen-perf-function u))
                     when (zerop (mod i 8)) do
                       (funcall freq-setter u
                                (let ((f (* 123 i)))
                                  (if (eq freq-setter #'set-ugen-test-1-freq*)
                                      (sample f)
                                      f)))
                       (set-ugen-test-1-nh u (- 50 i))
                     collect (sample->fixnum
                               (smp-ref (ugen-return-pointer u) 0)))))
        (let ((res (vals #'set-ugen-test-1-freq)))
          (funcall (ugen-reinit-function u) 100 123456 30)
          (let ((res2 (vals #'set-ugen-test-1-freq*)))
            (free u)
            (values (free-p u) (equal res res2) res)))))
  t t
  (123456 120149 120149 120149 120149 120149 120149 120149 120149 120149 -28964
   11973 -5919 -5167 3868 -6989 -350 -406 1051 -3246 -4440 -1074 -21 -2951
   -3946 -1188 -2763 -4047 -4353 -3300 -932 2352))

(with-ugen-test (ugen.7)
    (let (res res2 uu)
      (with-ugen-instance (u ugen-test-1 100 123456 30)
        (multiple-value-bind (ptr fn)
            (ugen-control-pointer u 'freq)
          (flet ((vals ()
                   (loop for i below 32
                         do (funcall (ugen-perf-function u))
                         when (zerop (mod i 8)) do
                           (setf (smp-ref ptr 0) (sample (* 123 i)))
                           (when fn (funcall fn))
                           (set-ugen-test-1-nh u (- 50 i))
                         collect (sample->fixnum
                                   (smp-ref (ugen-return-pointer u) 0)))))
            (setf res (vals))
            (funcall (ugen-reinit-function u) 100 123456 30)
            (setf res2 (vals))
            (setf uu u))))
      (values (free-p uu) (equal res res2) res))
  t t
  (123456 120149 120149 120149 120149 120149 120149 120149 120149 120149 -28964
   11973 -5919 -5167 3868 -6989 -350 -406 1051 -3246 -4440 -1074 -21 -2951
   -3946 -1188 -2763 -4047 -4353 -3300 -932 2352))
