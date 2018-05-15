(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! node-out-test-1 (freq amp pos)
  (foreach-channel (node-out (pan2 (sine freq amp 0) pos))))

(dsp! node-out-test-2 (freq amp pos)
  (initialize (assert (eq *node-enable-gain-p* t)))
  (foreach-channel
    (cout (* (the sample (node-gain (dsp-node)))
             (pan2 (sine freq amp 0) pos)))))

(dsp! done-test-1 (end dur)
  (setf (node-gain (dsp-node))
        (expon 0 end dur (lambda (n) (setf (done-p n) t)))))

(dsp! done-test-2 (amp fade-time)
  (with ((node nil))
    (declare (type (or node null) node))
    (initialize
      (setf node (reduce-warnings (done-test-1 amp fade-time))))
    (out (sine 440 (tick (if (done-p node) amp (node-gain node))) 0))))

(with-dsp-test (node-out.1 :channels 2
      :md5 #(122 192 115 127 47 7 39 111 86 25 115 36 107 224 247 78))
  (node-out-test-1 440 .5 .2 :id 123 :fade-time 0.8 :fade-curve 4)
  (at #[3/2 sec] #'node-out-test-1 880 .5 .8 :replace 123)
  (at #[5/2 sec] #'node-out-test-1 330 .5 .8 :replace 123 :fade-time 1)
  (at #[3.8 sec] #'node-out-test-1 660 .2 .5 :replace 123 :fade-time .4)
  (at #[4.5 sec] #'node-fade-out 123 .5 -4))

(with-dsp-test (node-out.2 :channels 2
      :md5 #(122 192 115 127 47 7 39 111 86 25 115 36 107 224 247 78))
  (setf *node-enable-gain-p* t)
  (at #[4.8 sec] (lambda () (setf *node-enable-gain-p* nil)))
  (node-out-test-2 440 .5 .2 :id 123 :fade-time 0.8 :fade-curve 4)
  (at #[3/2 sec] #'node-out-test-2 880 .5 .8 :replace 123)
  (at #[5/2 sec] #'node-out-test-2 330 .5 .8 :replace 123 :fade-time 1)
  (at #[3.8 sec] #'node-out-test-2 660 .2 .5 :replace 123 :fade-time .4
      :free-hook (list (lambda (n) n (assert (null *node-enable-gain-p*)))))
  (at #[4.5 sec] #'node-fade-out 123 .5 -4))

(with-dsp-test (node-out.3
      :md5 #(121 245 161 46 3 6 184 211 61 5 49 105 211 233 63 130))
  (done-test-2 .7 3))
