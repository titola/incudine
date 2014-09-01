(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-osc-test-1 ((buf buffer) freq amp phase)
  (out (osc buf freq amp phase :linear)))

(dsp! vug-osc-test-2 (fc fm index amp)
  (stereo (osc *sine-table* (+ fc (osc *sine-table* fm (* fm index) 0))
               (db->lin amp) 0 :cubic)))

(dsp! vug-osc-test-3 (frq0 frq1 amp afrq0 afrq1 dur)
  (stereo (osc *sine-table* (x-line frq0 frq1 dur #'free)
               (+ amp (osc *sine-table* (x-line afrq0 afrq1 dur #'free)
                           amp 0 :linear))
               0 :cubic)))

(dsp! vug-osc-test-4 ((buf buffer) freq amp phs-frq phs-dev)
  (out (osc buf freq amp (osc buf phs-frq phs-dev 0 :cubic) :cubic)))

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

(with-dsp-test (vug-osc.1
      :md5 #(196 80 85 127 53 96 164 210 199 49 67 184 70 43 255 107))
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
      :md5 #(145 167 145 97 161 168 242 36 143 141 66 245 37 66 121 159))
  (vug-osc-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-osc.pm.1
      :md5 #(88 247 96 6 32 65 93 95 28 177 11 205 90 176 210 160))
  (vug-osc-test-4 *sine-table* 440 .5 110 0 :id 1)
  (at #[2/3 s] #'set-control 1 :phs-dev (* 1/5 pi))
  (at #[4/3 s] #'set-control 1 :phs-dev (* 2/5 pi))
  (at #[2 s] #'set-control 1 :phs-dev (* 3/5 pi))
  (at #[8/3 s] #'set-control 1 :phs-dev (* 4/5 pi))
  (at #[4 s] #'set-control 1 :phs-dev pi))

(with-dsp-test (vug-sine.1
      :md5 #(225 41 118 182 36 172 231 7 76 60 161 222 67 21 199 30))
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
      :md5 #(105 182 64 57 150 240 34 179 230 127 199 123 145 75 107 3))
  (vug-sine-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-sine.pm.1
      :md5 #(149 147 206 168 162 200 14 248 27 168 97 50 136 205 24 86))
  (vug-sine-test-4 440 .5 110 0 :id 1)
  (at #[2/3 s] #'set-control 1 :phs-dev (* 1/5 pi))
  (at #[4/3 s] #'set-control 1 :phs-dev (* 2/5 pi))
  (at #[2 s] #'set-control 1 :phs-dev (* 3/5 pi))
  (at #[8/3 s] #'set-control 1 :phs-dev (* 4/5 pi))
  (at #[4 s] #'set-control 1 :phs-dev pi))

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
      :md5 #(22 185 8 55 177 34 64 15 65 124 215 6 139 207 243 27))
  (vug-oscr-test-3 50 4000 .35 1 400 +vug-test-duration-sec+))

(with-dsp-test (vug-oscr.fm.am.2 :channels 2
      :md5 #(240 12 130 93 94 80 167 130 101 2 16 169 26 53 98 168))
  (vug-oscr-test-4 50 4000 .35 1 400 +vug-test-duration-sec+))
