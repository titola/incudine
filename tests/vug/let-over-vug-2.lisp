(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! let-over-vug-4 (freq amp)
  (let* ((freq (+ freq (sine 8 (* freq .1d0))))
         (amp (db->linear amp))
         (oscil (sine freq amp)))
    (stereo oscil)))

(dsp! symbol-macrolet-over-vug-1 (freq amp)
  (symbol-macrolet ((oscil (sine f a))
                    (a (db->linear amp))
                    (f (+ freq (sine 8 (* freq .1d0)))))
    (stereo oscil)))

(dsp! multiple-value-bind-over-vug-1 (amp)
  (multiple-value-bind (freq amp) (values 440 (db->linear amp))
    (out (sine (sample freq) amp))))

(with-dsp-test (let-over-vug.4 :channels 2
      :md5 #(69 211 19 70 3 170 150 156 94 57 52 141 62 37 20 47))
  (let-over-vug-4 440 -6 :id 8)
  (at #[2.5 s] #'set-controls 8 :freq 1000 :amp -9))

(with-dsp-test (symbol-macrolet-over-vug.1 :channels 2
      :md5 #(69 211 19 70 3 170 150 156 94 57 52 141 62 37 20 47))
  (symbol-macrolet-over-vug-1 440 -6 :id 8)
  (at #[2.5 s] #'set-controls 8 :freq 1000 :amp -9))

(with-dsp-test (multiple-value-bind-over-vug.1
      :md5 #(37 158 61 252 199 18 43 43 209 247 230 58 169 135 54 70))
  (multiple-value-bind-over-vug-1 -9))
