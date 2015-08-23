(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! flet-over-vug-1 (freq amp f-percent a-percent)
  (with-samples ((m0 (flet ((scaler (x)
                              (* x 0.01)))
                       (scaler f-percent)))
                 (m1 (* a-percent 0.01)))
    (flet ((prova (fscale ascale)
             (declare (type sample fscale ascale))
             (sine (* fscale freq) (* ascale amp) 0)))
      (out (prova m0 m1)))))

(dsp! labels-over-vug-1 (freq amp f-percent a-percent)
  (with-samples ((m0 (labels ((scaler (x) (div100 x))
                              (div100 (x) (* x 0.01)))
                       (scaler f-percent)))
                 (m1 (* a-percent 0.01)))
    (labels ((prove (fscale ascale)
               (+ (prova (* fscale 3/2) (* ascale .8))
                  (sine (* fscale freq) (* ascale amp) 0)))
             (prova (fscale ascale)
               (declare (type sample fscale ascale))
               (sine (* fscale freq) (* ascale amp) 0)))
      (out (prove m0 m1)))))

(with-dsp-test (flet-over-vug.1
      :md5 #(133 30 145 103 143 149 213 125 6 156 63 127 54 219 38 60))
  (flet-over-vug-1 4400 1 10 30 :id 1)
  (at #[3 s] #'set-controls 1 :freq 3300 :amp .5)
  (at #[4 s] #'set-controls 1 :f-percent 20 :a-percent 40))

(with-dsp-test (labels-over-vug.1
      :md5 #(236 170 188 140 57 227 229 187 238 200 146 242 88 19 210 79))
  (labels-over-vug-1 4400 1 10 30 :id 1)
  (at #[3 s] #'set-controls 1 :freq 3300 :amp .5)
  (at #[4 s] #'set-controls 1 :f-percent 20 :a-percent 40))
