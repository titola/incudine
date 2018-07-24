(in-package :incudine-tests)

(dsp! balance-test-1 (scl dur)
  (with-samples ((in (noise-test scl)))
    (out (balance (bpf in (expon 100 4000 dur #'free) 100) in))))

(with-dsp-test (balance.1
      :md5 #(230 237 62 239 247 3 237 126 140 219 134 114 174 215 216 210))
  (balance-test-1 .4 5))
