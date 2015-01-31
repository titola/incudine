(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! buf-delay-s-test-1 ((buf buffer) (delay-samples fixnum))
  (with-samples ((in (impulse 1 .75)))
    (out (+ in (buf-delay-s buf in delay-samples)))))

(dsp! buf-delay-test-1 ((buf buffer) seconds)
  (with-samples ((in (impulse 1 .75)))
    (out (+ in (buf-delay buf in seconds)))))

(dsp! delay-s-test-1 ((delay-samples fixnum))
  (with-samples ((in (impulse 1 .75)))
    (out (+ in (delay-s in 65536 delay-samples)))))

(dsp! delay-test-1 (seconds)
  (with-samples ((in (impulse 1 .75)))
    (out (+ in (delay in 1 seconds)))))

(dsp! delay-test-2 (seconds fb)
  (with-samples ((in (* .75 (impulse))))
    (out (~ (delay (* fb (+ in it)) 1 seconds)))))

(with-dsp-test (buf-delay-s.1
      :md5 #(161 231 87 213 220 54 26 173 31 88 6 25 8 75 207 241))
  (buf-delay-s-test-1 *delay-buffer-test* 12345)
  (at #[2 s] #'free 0)
  (at #[2.5 s] #'buf-delay-s-test-1 *delay-buffer-test* 54321))

(with-dsp-test (buf-delay.1
      :md5 #(10 143 194 243 163 249 17 152 9 129 224 212 183 249 127 175))
  (buf-delay-test-1 *delay-buffer-test* .321)
  (at #[2 s] #'free 0)
  (at #[2.5 s] #'buf-delay-test-1 *delay-buffer-test* .88))

(with-dsp-test (delay-s.1
      :md5 #(161 231 87 213 220 54 26 173 31 88 6 25 8 75 207 241))
  (delay-s-test-1 12345)
  (at #[2 s] #'free 0)
  (at #[2.5 s] #'delay-s-test-1 54321))

(with-dsp-test (delay.1
      :md5 #(10 143 194 243 163 249 17 152 9 129 224 212 183 249 127 175))
  (delay-test-1 .321)
  (at #[2 s] #'free 0)
  (at #[2.5 s] #'delay-test-1 .88))

(with-dsp-test (delay.2
      :md5 #(32 90 19 246 190 65 34 90 178 82 228 148 146 23 142 162))
  (delay-test-2 .33 .8))
