(in-package :incudine-tests)

(dsp! reducible-var-1 (amp)
  (vuglet ((test (freq)
             ;; Precompute "(- +twopi+ 0)" during the simplification of
             ;; the VUG-VARIABLEs.
             (sin (wrap (* 10 (phasor freq 0)) 0 +twopi+))))
    (out (* amp (test 440)))))

(with-dsp-test (reducible-var.1
      :md5 #(130 182 107 207 33 216 214 78 203 110 12 176 35 62 93 168))
  (reducible-var-1 .8))
