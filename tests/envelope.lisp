(in-package :incudine-tests)

(defun envelope->time-level-list (env)
  (declare (type envelope env))
  (mapcar #'two-decimals
          (loop for i below (envelope-points env)
             collect (envelope-time env i)
             collect (envelope-level env i))))

(defun envelope-curves (env)
  (declare (type envelope env))
  (loop for i from 1 below (envelope-points env)
        collect (let ((curve (envelope-curve env i)))
                  (if (numberp curve)
                      (two-decimals curve)
                      curve))))

(defun envelope-test-1 (env)
  (values (envelope->time-level-list env)
          (envelope-curves env)))

(deftest envelope.1
    (let* ((env (make-envelope '(0 1 0) '(.5 .5)))
           (points (envelope-points env)))
      (free env)
      (values points (envelope-points env)))
  3 0)

(deftest envelope.2
    (envelope-test-1 (make-envelope '(0 1 .75 .42 0) '(.01 .08 2 .25)
                                    :curve :exp))
  (0.0 0.0 0.01 1.0 0.08 0.75 2.0 0.42 0.25 0.0)
  (:EXPONENTIAL :EXPONENTIAL :EXPONENTIAL :EXPONENTIAL))

(deftest envelope.3
    (envelope-test-1 (make-envelope '(0 1 .75 .42 0) '(.01 .08 2 .25)
                                    :curve '(:exp :lin :square)))
  (0.0 0.0 0.01 1.0 0.08 0.75 2.0 0.42 0.25 0.0)
  (:EXPONENTIAL :LINEAR :SQUARE :EXPONENTIAL))

(deftest envelope.4
    (let ((env (make-envelope '(0 1 .75 .42 0) '(.01 .08 2 .25)
                              :curve '(:exp :lin :square))))
      (setf (envelope-level env 2) .89
            (envelope-time env 2) 3
            (envelope-curve env 3) :cubic)
      (envelope-test-1 env))
  (0.0 0.0 0.01 1.0 3.0 0.89 2.0 0.42 0.25 0.0)
  (:EXPONENTIAL :LINEAR :CUBIC :EXPONENTIAL))

(deftest envelope-linen.1
    (envelope-test-1 (make-linen .5 1 1.5 :level .9))
  (0.0 0.0 0.5 0.9 1.0 0.9 1.5 0.0)
  (:LINEAR :LINEAR :LINEAR))

(deftest envelope-linen.2
    (let ((env (make-linen .5 1 1.5 :level .9)))
      (envelope-test-1 (linen env .25 3 0.5 :level .5)))
  (0.0 0.0 0.25 0.5 3.0 0.5 0.5 0.0)
  (:LINEAR :LINEAR :LINEAR))

(deftest envelope-perc.1
    (envelope-test-1 (make-perc .01 .25))
  (0.0 0.0 0.01 1.0 0.25 0.0)
  (-4.0 -4.0))

(deftest envelope-perc.2
    (let ((env (make-perc .01 .25)))
      (envelope-test-1 (perc env .25 .06)))
  (0.0 0.0 0.25 1.0 0.06 0.0)
  (-4.0 -4.0))

(deftest envelope-cutoff.1
    (envelope-test-1 (make-cutoff .25 :level .82))
  (0.0 0.82 0.25 0.0)
  (:EXPONENTIAL))

(deftest envelope-cutoff.2
    (let ((env (make-cutoff .25 :level .82)))
      (envelope-test-1 (cutoff env .02)))
  (0.0 1.0 0.02 0.0)
  (:EXPONENTIAL))

(deftest envelope-asr.1
    (envelope-test-1 (make-asr .16 .9 .35))
  (0.0 0.0 0.16 0.9 0.35 0.0)
  (-4.0 -4.0))

(deftest envelope-asr.2
    (let ((env (make-asr .16 .9 .35)))
      (envelope-test-1 (asr env 1.5 4 5)))
  (0.0 0.0 1.5 4.0 5.0 0.0)
  (-4.0 -4.0))

(deftest envelope-adsr.1
    (envelope-test-1 (make-adsr .16 .08 .82 .25))
  (0.0 0.0 0.16 1.0 0.08 0.82 0.25 0.0)
  (-4.0 -4.0 -4.0))

(deftest envelope-adsr.2
    (let ((env (make-adsr .16 .08 .82 .25)))
      (envelope-test-1 (adsr env 1.5 .5 .75 3)))
  (0.0 0.0 1.5 1.0 0.5 0.75 3.0 0.0)
  (-4.0 -4.0 -4.0))

(deftest envelope-dadsr.1
    (envelope-test-1 (make-dadsr 2.5 .16 .08 .82 .25))
  (0.0 0.0 2.5 0.0 0.16 1.0 0.08 0.82 0.25 0.0)
  (-4.0 -4.0 -4.0 -4.0))

(deftest envelope-dadsr.2
    (let ((env (make-dadsr 2.5 .16 .08 .82 .25)))
      (envelope-test-1 (dadsr env .25 1.4 .04 .75 2.2)))
  (0.0 0.0 0.25 0.0 1.4 1.0 0.04 0.75 2.2 0.0)
  (-4.0 -4.0 -4.0 -4.0))

(deftest envelope-scale
    (let ((env (make-envelope '(440 2500 880) '(.5 2.5))))
      (envelope-test-1 (scale env .01)))
  (0.0 4.4 0.5 25.0 2.5 8.8)
  (:LINEAR :LINEAR))

(deftest envelope-normalize
    (let ((env (make-envelope '(440 2500 880) '(.5 2.5))))
      (envelope-test-1 (normalize env 2)))
  (0.0 0.35 0.5 2.0 2.5 0.7)
  (:LINEAR :LINEAR))

(deftest envelope-rescale
    (let ((env (make-envelope '(440 2500 880) '(.5 2.5))))
      (envelope-test-1 (rescale env 220 4000)))
  (0.0 220.0 0.5 4000.0 2.5 1027.38)
  (:LINEAR :LINEAR))
