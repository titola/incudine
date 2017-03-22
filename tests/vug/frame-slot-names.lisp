(in-package :incudine-tests)

(dsp! frame-slot-names-1 ()
  (with-samples ((freq 440) (amp .5) (phase +half-pi+))
    (declare (preserve freq amp phase))
    (with ((f (get-pointer freq)))
      (out (sine (smp-ref f 0) (smp-ref f 1) (smp-ref f 2))))))

(with-dsp-test (frame-slot-names.1
      :md5 #(24 107 78 114 149 84 150 89 11 140 197 121 222 105 175 143))
  (frame-slot-names-1))
