(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)
(enable-sharp-t-syntax)

(dsp! vuglet-1 (freq amp)
  (vuglet ((osc (freq amp)
             (* amp (sin (* (phasor freq 0) +twopi+))))
           (fm (fcar fmod amp (k fixnum))
             (osc (+ fcar (osc fmod (* fcar (/ k)))) amp))
           (lp (in)
             (* 0.5 (+ in (delay1 in))))
           (lp4 (in) #4t(lp in)))
    (with-samples ((f1 freq)
                   (f2 (- 15000 f1))
                   (g (* amp .5)))
      (out (+ (lp (fm f1 111 g 8)) (lp4 (fm f2 70 g 18)))))))

(with-dsp-test (vuglet.1
      :md5 #(252 112 117 44 143 169 89 77 28 157 179 116 124 36 84 115))
  (vuglet-1 440 .8 :id 8)
  (at #[1.2 s] #'set-control 8 :freq 2000)
  (at #[3.5 s] #'set-control 8 :freq 200))
