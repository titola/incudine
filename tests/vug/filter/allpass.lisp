(in-package :incudine-tests)

(dsp! allpass-test-1 ((dt1 fixnum) (dt2 fixnum) (dt3 fixnum))
  (vuglet ((ap ((dt fixnum) in)
             (allpass-s in 2000 dt .7)))
    (out (ap dt3 (ap dt2 (ap dt1 (impulse)))))))

(dsp! allpass-test-2 (dt1 dt2 dt3)
  (vuglet ((ap (dt in)
             (allpass in (* 2000 *sample-duration*) dt .7)))
    (out (ap dt3 (ap dt2 (ap dt1 (impulse)))))))

(with-dsp-test (allpass.1
      :md5 #(138 167 52 162 42 159 51 111 160 132 247 10 64 82 130 123))
  (allpass-test-1 1601 541 179))

(with-dsp-test (allpass.2
      :md5 #(138 167 52 162 42 159 51 111 160 132 247 10 64 82 130 123))
  (flet ((s (x) (* x *sample-duration*)))
    (allpass-test-2 (s 1601) (s 541) (s 179))))
