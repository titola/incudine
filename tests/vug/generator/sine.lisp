(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-sine-test-1 (freq amp phase)
  (out (sine freq amp phase)))

(dsp! vug-sine-test-2 (fc fm index amp)
  (stereo (sine (+ fc (sine fm (* fm index) 0)) (db->lin amp) 0)))

(dsp! vug-sine-test-3 (frq0 frq1 amp afrq0 afrq1 dur)
  (stereo (sine (x-line frq0 frq1 dur #'free)
                (+ amp (sine (x-line afrq0 afrq1 dur #'free) amp 0))
                0)))

(dsp! vug-sine-test-4 (freq amp phs-frq phs-dev)
  (out (sine freq amp (sine phs-frq phs-dev 0))))

(with-dsp-test (vug-sine.1
      :md5 #(101 132 59 132 40 118 106 208 181 47 167 103 207 183 22 12))
  (vug-sine-test-1 440 .7 0 :id 123)
  (at #[1 sec] #'set-control 123 :freq 880)
  (at #[5/2 sec] #'set-controls 123 :freq 220 :amp .25)
  (at #[3.7 seconds] (lambda () (setf (control-value 123 'freq) 660))))

(with-dsp-test (vug-sine.2
      :md5 #(107 21 92 190 246 186 152 225 219 15 246 100 5 205 133 136))
  (vug-sine-test-1 440 .5 pi :id 1)
  (at #[1 s] #'set-control 1 :phase +half-pi+)
  (at #[2 s] #'set-control 1 :phase (- +half-pi+))
  (at #[3 s] #'set-control 1 :phase +twopi+))

(with-dsp-test (vug-sine.3
      :md5 #(208 81 207 249 39 212 206 40 171 114 33 94 67 162 251 43))
  ;; Silence.
  (vug-sine-test-1 440 .5 0)
  (vug-sine-test-1 440 .5 pi))

(with-dsp-test (vug-sine.fm.1 :channels 2
      :md5 #(132 89 66 29 234 85 7 68 217 179 104 198 135 14 35 189))
  (vug-sine-test-2 440 110 0 -6 :id 123)
  (loop for time in (list #[1/2 s] #[1 s] #[3/2 s] #[2 s] #[5/2 s])
        for index from 1 by .5
        for amp from 6
        do (at time #'set-controls 123 :index index :amp (- amp))))

(with-dsp-test (vug-sine.fm.am.1 :channels 2
      :md5 #(43 16 206 140 123 93 192 110 57 68 227 96 150 201 239 160))
  (vug-sine-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-sine.pm.1
      :md5 #(149 147 206 168 162 200 14 248 27 168 97 50 136 205 24 86))
  (vug-sine-test-4 440 .5 110 0 :id 1)
  (at #[2/3 s] #'set-control 1 :phs-dev (* 1/5 pi))
  (at #[4/3 s] #'set-control 1 :phs-dev (* 2/5 pi))
  (at #[2 s] #'set-control 1 :phs-dev (* 3/5 pi))
  (at #[8/3 s] #'set-control 1 :phs-dev (* 4/5 pi))
  (at #[4 s] #'set-control 1 :phs-dev pi))
