(in-package :incudine-tests)

(dsp! let-over-vug-1 (amp)
  (let ((freq 440))
    (out (sine (sample freq) amp 0))))

(dsp! let-over-vug-2 (amp)
  (with-samples ((freq (let ((freq 8))
                         (+ 440 (sine (sample freq) 40 0)))))
    (out (sine freq amp 0))))

(dsp! let-over-vug-3 (amp f0 f1)
  ;; Bad style because FREQ (not the LET-binding) is performance-time
  ;; (it depends on SINE VUG), so the LET-binding FREQ and its value
  ;; `(+ F0 F1)', expanded within the body of the VUG, are
  ;; performance-time.  F0 and F1 are always summed, also if they
  ;; don't change.  If we use WITH instead of LET, the sum `(+ F0 F1)'
  ;; is updated only after the change of F0 or F1, because the value
  ;; bound to a variable in WITH is automatically moved out of the
  ;; performance function when necessary.
  (with-samples ((freq (let ((freq (+ f0 f1)))
                         (+ 440 (sine (sample freq) 40 0)))))
    (out (sine freq amp 0))))

(dsp! let-init-1 (amp)
  (with-samples ((freq (let ((freq 440))
                         (sample freq))))
    (out (sine freq amp 0))))

(dsp! let-init-2 (amp f0 f1)
  (with-samples ((freq (let ((freq (+ f0 f1)))
                         (sample freq))))
    (out (sine freq amp 0))))

(with-dsp-test (let-over-vug.1
      :md5 #(13 165 85 102 64 98 53 34 161 88 38 4 70 123 22 209))
  (let-over-vug-1 .5))

(with-dsp-test (let-over-vug.2
      :md5 #(137 235 29 219 28 176 240 98 112 139 161 157 182 27 232 83))
  (let-over-vug-2 .5))

(with-dsp-test (let-over-vug.3
      :md5 #(137 235 29 219 28 176 240 98 112 139 161 157 182 27 232 83))
  (let-over-vug-3 .5 3 5))

(with-dsp-test (let-init.1
      :md5 #(13 165 85 102 64 98 53 34 161 88 38 4 70 123 22 209))
  (let-init-1 .5))

(with-dsp-test (let-init.2
      :md5 #(13 165 85 102 64 98 53 34 161 88 38 4 70 123 22 209))
  (let-init-2 .5 110 330))
