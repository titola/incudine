(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-oscr-test-1 (freq amp)
  (out (oscr freq amp)))

(dsp! vug-oscr-test-2 (fc fm index amp)
  (stereo (oscr (+ fc (oscr fm (* fm index))) (db->lin amp))))

(dsp! vug-oscr-test-3 (frq0 frq1 amp afrq0 afrq1 dur)
  (stereo (oscr (x-line frq0 frq1 dur #'free)
                (+ amp (oscr (x-line afrq0 afrq1 dur #'free) amp)))))

(dsp! vug-oscr-test-4 (frq0 frq1 amp afrq0 afrq1 dur)
  (with-samples ((am (+ amp (oscr (x-line afrq0 afrq1 dur #'free) amp))))
    (multiple-sample-bind (sin cos)
        (oscrq (x-line frq0 frq1 dur #'free))
      (out (* am sin) (* am cos)))))

(with-dsp-test (vug-oscr.1
      :md5 #(52 30 52 249 0 198 98 93 64 211 5 18 88 125 34 98))
  (vug-oscr-test-1 440 .7 :id 123)
  (at #[1 sec] #'set-control 123 :freq 880)
  (at #[5/2 sec] #'set-controls 123 :freq 220 :amp .25)
  (at #[3.7 seconds] (lambda () (setf (control-value 123 'freq) 660))))

(with-dsp-test (vug-oscr.fm.1 :channels 2 :mult 1d6
      :md5 #(223 128 48 216 74 59 56 12 188 171 71 55 7 113 193 122))
  (vug-oscr-test-2 440 110 0 -6 :id 123)
  (loop for time in (list #[1/2 s] #[1 s] #[3/2 s] #[2 s] #[5/2 s])
        for index from 1 by .5
        for amp from 6
        do (at time #'set-controls 123 :index index :amp (- amp))))

(with-dsp-test (vug-oscr.fm.am.1 :channels 2
      :md5 #(196 215 27 219 114 123 99 91 101 28 210 61 228 63 34 72))
  (vug-oscr-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-oscr.fm.am.2 :channels 2
      :md5 #(218 245 5 125 14 193 74 93 31 204 86 34 33 192 65 115))
  (vug-oscr-test-4 50 4000 .35 1 400 +vug-test-duration-sec+))
