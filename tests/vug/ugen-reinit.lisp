(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-ugen line* sample (start end dur (done-action function))
    (line start end dur done-action))

  (define-ugen x-line* sample (start end dur (done-action function))
    (x-line start end dur done-action))

  (define-ugen envgen* sample ((env envelope) gate time-scale
                               (done-action function))
    (envgen env gate time-scale done-action))

  (define-ugen sinosc-1 sample (freq amp)
    (sine freq amp 0))

  (define-ugen sinosc-2 sample (freq amp)
    (oscr freq amp))

  (define-ugen sinosc-fadein* sample (f0 f1 amp dur)
    (* (line* 0 amp (1+ dur) #'free)
       (sinosc-1 (x-line* f0 f1 dur #'reinit) amp)))

  (define-ugen sinosc-fadein sample (f0 f1 amp dur)
    (declare (inline line* x-line* sinosc-1))
    (* (line* 0 amp (1+ dur) #'free)
       (sinosc-1 (x-line* f0 f1 dur #'reinit) amp)))

  (define-ugen sinosc-perc* sample (amp dur)
    (* (envgen* (make-local-perc .5 .5) 1 dur
                (reduce-warnings
                  (lambda (node) (reinit node amp dur))))
       (sinosc-2 1000 amp)))

  (define-ugen sinosc-perc sample (amp dur)
    (declare (inline envgen* sinosc-2))
    (* (envgen* (make-local-perc .5 .5) 1 dur
                (reduce-warnings
                  (lambda (node) (reinit node amp dur))))
       (sinosc-2 1000 amp))))

(dsp! ugen-reinit-test-1 (f0 f1 amp dur)
  (out (sinosc-fadein f0 f1 amp dur)))

(dsp! ugen-reinit-test-2 (f0 f1 amp dur)
  (out (sinosc-fadein* f0 f1 amp dur)))

(dsp! ugen-reinit-test-3 (amp dur)
  (stereo (sinosc-perc amp dur)))

(dsp! ugen-reinit-test-4 (amp dur)
  (stereo (sinosc-perc* amp dur)))

(with-dsp-test (ugen-reinit.1
      :md5 #(111 65 233 19 87 24 110 145 222 109 13 187 206 192 236 177))
  (ugen-reinit-test-1 50 4000 .7 2 :id 123)
  (at #[2.1 sec] #'set-control 123 :dur .4))

(with-dsp-test (ugen-reinit.2
      :md5 #(111 65 233 19 87 24 110 145 222 109 13 187 206 192 236 177))
  (ugen-reinit-test-2 50 4000 .7 2 :id 123)
  (at #[2.1 sec] #'set-control 123 :dur .4))

(with-dsp-test (ugen-reinit.3 :channels 2
      :md5 #(155 94 211 222 231 35 83 232 205 190 160 30 126 202 245 238))
  (ugen-reinit-test-3 .75 1 :id 123)
  (at #[1/2 sec] #'set-control 123 :dur .1)
  (at #[2 sec] #'set-control 123 :dur .3)
  (at #[3 sec] #'set-control 123 :dur .5))

(with-dsp-test (ugen-reinit.4 :channels 2
      :md5 #(155 94 211 222 231 35 83 232 205 190 160 30 126 202 245 238))
  (ugen-reinit-test-4 .75 1 :id 123)
  (at #[1/2 sec] #'set-control 123 :dur .1)
  (at #[2 sec] #'set-control 123 :dur .3)
  (at #[3 sec] #'set-control 123 :dur .5))
