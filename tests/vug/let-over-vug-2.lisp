(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! let-over-vug-4 (freq amp)
  (let* ((freq (+ freq (sine 8 (* freq .1) 0)))
         (amp (db->lin amp))
         (oscil (sine freq amp 0)))
    (stereo oscil)))

(dsp! symbol-macrolet-over-vug-1 (freq amp)
  (symbol-macrolet ((oscil (sine f a 0))
                    (a (db->lin amp))
                    (f (+ freq (sine 8 (* freq .1) 0))))
    (stereo oscil)))

(dsp! multiple-value-bind-over-vug-1 (amp)
  (multiple-value-bind (freq amp)
      (values 440 (db->lin amp))
    (out (sine (sample freq) amp 0))))

(with-dsp-test (let-over-vug.4 :channels 2
      :md5 #(197 83 21 245 254 199 117 3 159 59 12 84 8 83 73 210))
  (let-over-vug-4 440 -6 :id 8)
  (at #[2.5 s] #'set-controls 8 :freq 1000 :amp -9))

(with-dsp-test (symbol-macrolet-over-vug.1 :channels 2
      :md5 #(197 83 21 245 254 199 117 3 159 59 12 84 8 83 73 210))
  (symbol-macrolet-over-vug-1 440 -6 :id 8)
  (at #[2.5 s] #'set-controls 8 :freq 1000 :amp -9))

(with-dsp-test (multiple-value-bind-over-vug.1
      :md5 #(245 146 33 162 174 200 133 207 232 236 31 195 222 173 104 150))
  (multiple-value-bind-over-vug-1 -9))
