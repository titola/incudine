(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! reinit-test-1 (f0 f1 amp dur)
  (out (* (line 0 amp (1+ dur) #'free)
          (sine (x-line f0 f1 dur #'reinit) amp 0))))

(dsp! reinit-test-2 (f0 f1 amp dur)
  (out (* (line 0 amp (1+ dur) #'free)
          (sine (x-line f0 f1 dur
                        (reduce-warnings
                          (lambda (node) (reinit node f0 f1 amp dur))))
                amp 0))))

(dsp! reinit-test-3 (amp dur)
  (stereo (* (envgen (make-local-perc .5 .5) 1 dur
                     (reduce-warnings
                       (lambda (node) (reinit node amp dur))))
             (oscr 1000 amp))))

(with-dsp-test (reinit.1
      :md5 #(108 165 75 249 54 118 11 58 217 239 38 143 214 169 239 20))
  (reinit-test-1 50 4000 .7 2 :id 123)
  (at #[2.1 sec] #'set-control 123 :dur .4))

(with-dsp-test (reinit.2
      :md5 #(195 148 233 196 11 200 79 45 59 243 109 253 158 172 165 67))
  (reinit-test-2 50 4000 .7 2 :id 123)
  (at #[2.1 sec] #'set-control 123 :dur .4))

(with-dsp-test (reinit.3 :channels 2
      :md5 #(171 189 68 111 168 184 150 173 223 103 82 68 222 137 54 49))
  (reinit-test-3 .75 1 :id 123)
  (at #[1/2 sec] #'set-control 123 :dur .1)
  (at #[2 sec] #'set-control 123 :dur .3)
  (at #[3 sec] #'set-control 123 :dur .5))
