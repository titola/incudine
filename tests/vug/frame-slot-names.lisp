(in-package :incudine-tests)

(dsp! frame-slot-names-1 ()
  (with-samples ((freq 440) (amp .5) (phase +half-pi+))
    (declare (preserve freq amp phase))
    (with ((f (get-pointer freq)))
      (out (sine (smp-ref f 0) (smp-ref f 1) (smp-ref f 2))))))

(dsp! pointed-var-1 (amp)
  (with-samples ((a amp) (b +sample-zero+))
    (with ((pa (get-pointer a))
           (pb (get-pointer b))
           (i 0))
      (declare (pointer pa pb) (fixnum i))
      (when (= i 48000)
        (setf pa pb)
        ;; PA doesn't depend on AMP
        (reduce-warnings (set-control (dsp-node) 'amp (* a 2))))
      (incf i)
      ;; The output is zero after 48000 samples.
      (out (smp-ref pa 0)))))

(with-dsp-test (frame-slot-names.1
      :md5 #(24 107 78 114 149 84 150 89 11 140 197 121 222 105 175 143))
  (frame-slot-names-1))

(with-dsp-test (pointed-var.1
      :md5 #(28 147 131 9 232 230 31 246 63 72 207 82 162 189 137 8))
  (pointed-var-1 .4))
