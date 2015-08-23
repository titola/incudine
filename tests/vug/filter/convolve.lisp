(in-package :incudine-tests)

(dsp! direct-convolve-test-1 ((buf buffer))
  (out (direct-convolve (buzz 100 .5 200) buf)))

(with-dsp-test (direct-convolve.1
      :md5 #(37 30 104 242 95 25 79 197 191 33 139 41 220 212 243 59))
  (direct-convolve-test-1 *filter-buffer-test-1*))

(with-dsp-test (direct-convolve.2
      :md5 #(99 134 236 142 185 228 22 29 47 66 227 223 168 13 234 183))
  (direct-convolve-test-1 *filter-buffer-test-2*))
