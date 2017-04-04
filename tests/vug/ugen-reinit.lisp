(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-ugen line* sample (start end dur (done-action function))
    (line start end dur done-action))

  (define-ugen expon* sample (start end dur (done-action function))
    (expon start end dur done-action))

  (define-ugen envelope* sample ((env envelope) gate time-scale
                                 (done-action function))
    (envelope env gate time-scale done-action))

  (define-ugen sinosc-1 sample (freq amp)
    (sine freq amp 0))

  (define-ugen sinosc-2 sample (freq amp)
    (oscr freq amp))

  (define-ugen sinosc-fadein* sample (f0 f1 amp dur)
    (* (line* 0 amp (1+ dur) #'free)
       (sinosc-1 (expon* f0 f1 dur #'reinit) amp)))

  (define-ugen sinosc-fadein sample (f0 f1 amp dur)
    (declare (inline line* expon* sinosc-1))
    (* (line* 0 amp (1+ dur) #'free)
       (sinosc-1 (expon* f0 f1 dur #'reinit) amp)))

  (define-ugen sinosc-perc* sample (amp dur)
    (* (envelope* (make-local-perc .5 .5) 1 dur
                  (reduce-warnings
                    (lambda (node) (reinit node amp dur))))
       (sinosc-2 1000 amp)))

  (define-ugen sinosc-perc sample (amp dur)
    (declare (inline envelope* sinosc-2))
    (* (envelope* (make-local-perc .5 .5) 1 dur
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
      :md5 #(35 60 65 86 72 80 25 81 199 191 1 202 233 115 8 38))
  (ugen-reinit-test-3 .75 1 :id 123)
  (at #[1/2 sec] #'set-control 123 :dur .1)
  (at #[2 sec] #'set-control 123 :dur .3)
  (at #[3 sec] #'set-control 123 :dur .5))

(with-dsp-test (ugen-reinit.4 :channels 2
      :md5 #(35 60 65 86 72 80 25 81 199 191 1 202 233 115 8 38))
  (ugen-reinit-test-4 .75 1 :id 123)
  (at #[1/2 sec] #'set-control 123 :dur .1)
  (at #[2 sec] #'set-control 123 :dur .3)
  (at #[3 sec] #'set-control 123 :dur .5))
