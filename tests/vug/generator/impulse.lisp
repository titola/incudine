(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-impulse-test-1 ()
  (out (impulse)))

(dsp! vug-impulse-test-2 (freq amp phase)
  (stereo (impulse freq amp phase)))

(dsp! vug-impulse-test-3 (freq amp fmod dev)
  (out (impulse (+ freq (sine fmod dev 0)) amp)))

(dsp! vug-impulse-test-4 (frq0 frq1 amp afrq0 afrq1 dur)
  (stereo (impulse (x-line frq0 frq1 dur #'free)
                   (+ amp (sine (x-line afrq0 afrq1 dur #'free) amp 0)))))

(dsp! vug-impulse-test-5 (freq amp phs-freq phs-dev)
  (stereo (impulse freq amp (+ phs-dev (sine phs-freq phs-dev 0)))))

(with-dsp-test (vug-single-impulse.1
      :md5 #(65 136 30 204 48 137 94 90 143 250 19 231 98 48 219 209))
  (vug-impulse-test-1))

(with-dsp-test (vug-impulse.1 :channels 2
      :md5 #(22 55 79 50 94 51 111 102 251 55 66 216 239 28 90 54))
  (vug-impulse-test-2 1 0.890 0.123)
  (vug-impulse-test-2 1 0.789 0.234)
  (vug-impulse-test-2 1 0.678 0.345)
  (vug-impulse-test-2 1 0.567 0.456)
  (vug-impulse-test-2 1 0.456 0.567)
  (vug-impulse-test-2 1 0.345 0.678)
  (vug-impulse-test-2 1 0.234 0.789)
  (vug-impulse-test-2 1 0.123 0.890))

(with-dsp-test (vug-impulse.fm.1
      :md5 #(4 139 81 86 203 190 24 54 202 96 126 49 251 126 153 126))
  (vug-impulse-test-3 50 0.5 0.4 30 :id 1)
  (at #[2.3 s] #'set-controls 1 :freq 100 :fmod .1 :dev 70)
  (at #[4 s] #'set-controls 1 :freq 20 :fmod 3 :dev 10 :amp .3))

(with-dsp-test (vug-impulse.fm.am.1 :channels 2
      :md5 #(242 166 54 194 45 187 134 139 196 178 149 112 231 40 100 213))
  (vug-impulse-test-4 10 100 .35 20 1 +vug-test-duration-sec+))

(with-dsp-test (vug-impulse.pm.1 :channels 2
      :md5 #(19 227 70 8 73 41 11 2 97 177 31 237 168 188 165 74))
  (vug-impulse-test-5 5 .5 1 0.3))
