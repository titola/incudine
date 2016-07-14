(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-osc-test-1 ((buf buffer) freq amp phase)
  (out (osc buf freq amp phase :linear)))

(dsp! vug-osc-test-2 (fc fm index amp)
  (stereo (osc *sine-table* (+ fc (osc *sine-table* fm (* fm index) 0))
               (db->lin amp) 0 :cubic)))

(dsp! vug-osc-test-3 (frq0 frq1 amp afrq0 afrq1 dur)
  (stereo (osc *sine-table* (expon frq0 frq1 dur #'free)
               (+ amp (osc *sine-table* (expon afrq0 afrq1 dur #'free)
                           amp 0 :linear))
               0 :cubic)))

(dsp! vug-osc-test-4 ((buf buffer) freq amp phs-frq phs-dev)
  (out (osc buf freq amp (osc buf phs-frq phs-dev 0 :cubic) :cubic)))

(with-dsp-test (vug-osc.1
      :md5 #(160 229 58 63 53 36 236 190 96 200 148 1 37 166 191 253))
  (vug-osc-test-1 *sine-table* 440 .7 0 :id 123)
  (at #[1 sec] #'set-control 123 :freq 880)
  (at #[5/2 sec] #'set-controls 123 :freq 220 :amp .25)
  (at #[3.7 seconds] (lambda () (setf (control-value 123 'freq) 660))))

(with-dsp-test (vug-osc.2
      :md5 #(126 175 190 81 61 152 207 190 77 169 44 203 132 102 114 31))
  (vug-osc-test-1 *sine-table* 440 .5 pi :id 1)
  (at #[1 s] #'set-control 1 :phase +half-pi+)
  (at #[2 s] #'set-control 1 :phase (- +half-pi+))
  (at #[3 s] #'set-control 1 :phase +twopi+))

(with-dsp-test (vug-osc.3
      :md5 #(208 81 207 249 39 212 206 40 171 114 33 94 67 162 251 43))
  ;; Silence.
  (vug-osc-test-1 *sine-table* 440 .5 0)
  (vug-osc-test-1 *sine-table* 440 .5 pi))

(with-dsp-test (vug-osc.fm.1 :channels 2
      :md5 #(2 229 123 17 97 42 50 54 199 22 8 64 65 126 85 75))
  (vug-osc-test-2 440 110 0 -6 :id 123)
  (loop for time in (list #[1/2 s] #[1 s] #[3/2 s] #[2 s] #[5/2 s])
        for index from 1 by .5
        for amp from 6
        do (at time #'set-controls 123 :index index :amp (- amp))))

(with-dsp-test (vug-osc.fm.am.1 :channels 2
      :md5 #(60 201 63 28 177 236 172 107 19 37 101 191 4 62 217 71))
  (vug-osc-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-osc.pm.1
      :md5 #(88 247 96 6 32 65 93 95 28 177 11 205 90 176 210 160))
  (vug-osc-test-4 *sine-table* 440 .5 110 0 :id 1)
  (at #[2/3 s] #'set-control 1 :phs-dev (* 1/5 pi))
  (at #[4/3 s] #'set-control 1 :phs-dev (* 2/5 pi))
  (at #[2 s] #'set-control 1 :phs-dev (* 3/5 pi))
  (at #[8/3 s] #'set-control 1 :phs-dev (* 4/5 pi))
  (at #[4 s] #'set-control 1 :phs-dev pi))
