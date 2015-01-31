(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! buf-tap-test-1 ((buf buffer) (dt1 fixnum) (dt2 fixnum)
                      (dt3 fixnum) (dt4 fixnum))
  (with ((in (impulse 1 .75))
         (mask (buffer-mask buf))
         (data (buffer-data buf))
         (tap1-head (logand (- dt2) mask))
         (tap2-head (logand (- dt3) mask))
         (tap3-head (logand (- dt4) mask)))
    (declare (type sample in) (type foreign-pointer data)
             (type fixnum mask tap1-head tap2-head tap3-head))
    (out (prog1 (+ in (buf-delay-s buf in dt1)
                   (smp-ref data tap1-head)
                   (smp-ref data tap2-head)
                   (smp-ref data tap3-head))
           (setf tap1-head (logand (1+ tap1-head) mask))
           (setf tap2-head (logand (1+ tap2-head) mask))
           (setf tap3-head (logand (1+ tap3-head) mask))))))

(dsp! buf-tap-test-2 ((buf buffer) dt1 dt2 dt3 dt4)
  (with ((in (impulse 1 .75))
         (write-head 0))
    (declare (type sample in) (type fixnum write-head))
    (out (+ in (buf-vdelay buf in dt1 nil write-head)
            (vtap buf dt2 write-head)
            (vtap buf dt3 write-head)
            (vtap buf dt4 write-head)))))

(with-dsp-test (buf-delay-tap.1
      :md5 #(29 139 102 102 51 163 237 95 87 120 56 120 147 90 32 63))
  (buf-tap-test-1 *delay-buffer-test* 12345 54321 23145 33333))

(with-dsp-test (buf-delay-tap.2
      :md5 #(29 139 102 102 51 163 237 95 87 120 56 120 147 90 32 63))
  (buf-tap-test-2 *delay-buffer-test* 0.2571875d0 1.1316875d0
                  0.4821875d0 0.6944374999999999d0))
