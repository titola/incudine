(in-package :incudine-tests)

(dsp! local-shadowing-1 (freq amp p0)
  (flet ((y (in) (sin (* in pi)))
         (w (in) (wrap in -1 1)))
    (macrolet ((y (in) `(sin (* ,in +twopi+))))
      (with-samples ((inc (* 2 freq *sample-duration*))
                     (p1 (~ (w (+ it inc)))))
        (out (* amp (y (w (+ p0 p1)))))))))

(dsp! local-shadowing-2 (freq amp p0)
  (macrolet ((y (in) `(sin (* ,in +twopi+))))
    (flet ((y (in) (sin (* in pi)))
           (w (in) (wrap in -1 1)))
      (with-samples ((inc (* 2 freq *sample-duration*))
                     (p1 (~ (w (+ it inc)))))
        (out (* amp (y (w (+ p0 p1)))))))))

(dsp! local-shadowing-3 (freq amp p0)
  (macrolet ((y (in) `(sin (* ,in +twopi+))))
    (flet ((w (in) (wrap (* in 3) -1 1)))
      (macrolet ((y (in) `(sin (* ,in pi))))
        (flet ((w (in) (wrap in -1 1)))
          (with-samples ((inc (* 2 freq *sample-duration*))
                         (p1 (~ (w (+ it inc)))))
            (out (* amp (y (w (+ p0 p1)))))))))))

(with-dsp-test (local-shadowing.1
      :md5 #(80 89 82 194 35 84 110 120 182 219 73 10 15 136 43 170))
  (local-shadowing-1 440 .7 0))

(with-dsp-test (local-shadowing.2
      :md5 #(125 204 113 162 173 222 13 29 76 164 212 255 27 46 33 110))
  (local-shadowing-2 440 .7 0))

(with-dsp-test (local-shadowing.3
      :md5 #(125 204 113 162 173 222 13 29 76 164 212 255 27 46 33 110))
  (local-shadowing-3 440 .7 0))
