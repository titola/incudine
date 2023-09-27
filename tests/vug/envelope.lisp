(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(defvar *env-test* (make-envelope '(0 1 0) '(.5 .5)))

(pushnew (lambda ()
           (edit-envelope *env-test* '(0 1 0) '(.5 .5) :curve :lin
                          :loop-node -1 :release-node -1))
         *test-hook*)

(dsp! ramp-test-1 (start end dur)
  (out (* (line start end dur #'identity) (noise-test 1))))

(dsp! expon-test-1 (start end dur)
  (out (* (expon start end dur #'identity) (noise-test 1))))

(dsp! vug-envelope-test ((env envelope) gain gate dur)
  (out (* (db->linear gain) (envelope env gate dur #'free))))

(dsp! vug-breakpoint-env-test ((bp cons) gain (base real) dur)
  (out (* (db->linear gain)
          (envelope (breakpoints->env bp :base base :duration 1)
                    1 dur #'free))))

(with-dsp-test (vug-envelope.1
      :md5 #(207 59 34 18 11 54 37 24 225 154 130 75 218 104 171 195))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.2
      :md5 #(25 78 108 47 214 123 111 208 218 133 190 230 125 5 47 35))
  (setf (envelope-curve *env-test* 1) :exp)
  (setf (envelope-curve *env-test* 2) :linear)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.3
      :md5 #(17 178 73 79 28 210 38 202 78 99 168 65 32 17 165 82))
  (setf (envelope-curve *env-test* 1) 4)
  (setf (envelope-curve *env-test* 2) -4)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.4
      :md5 #(95 15 185 170 243 117 0 221 68 96 52 154 101 77 138 218))
  (set-envelope-base *env-test* 7)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.5
      :md5 #(8 181 105 41 233 130 13 70 111 182 190 223 51 166 116 173))
  (edit-envelope *env-test* '(0 1 0) '(.2d0 .8d0) :curve (list :cub :sqr))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.6
      :md5 #(94 215 27 220 95 56 113 85 73 122 59 168 216 225 196 203))
  (edit-envelope *env-test* '(0 1 .3d0 .15d0) '(.2d0 .3d0 .5d0) :base .2d0)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-linen.1
      :md5 #(143 92 85 65 45 112 159 97 81 7 143 86 78 59 163 27))
  (edit-envelope *env-test* :linen '(.3d0 3.7d0 1))
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-linen.2
      :md5 #(13 122 206 113 199 20 32 4 120 188 151 236 254 215 103 143))
  (edit-envelope *env-test* :linen '(.3d0 3.7d0 1) :curve :sin)
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-perc.1
      :md5 #(27 234 125 76 239 25 243 75 112 151 238 188 9 51 163 119))
  (edit-envelope *env-test* :perc '(1d-3 1))
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-cutoff.1
      :md5 #(26 129 223 155 146 232 145 92 43 204 43 164 84 157 138 16))
  (edit-envelope *env-test* :cutoff 2)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-cutoff.2
      :md5 #(120 10 210 225 239 136 140 30 111 170 58 224 9 61 5 64))
  (edit-envelope *env-test* :cutoff 2 :curve 8)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-asr.1
      :md5 #(202 175 59 40 149 155 206 51 51 207 75 120 121 212 177 236))
  (edit-envelope *env-test* :asr '(1 1 1.5))
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[3 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-adsr.1
      :md5 #(214 193 129 210 151 142 238 177 146 211 249 107 43 82 81 231))
  (edit-envelope *env-test* :adsr '(.2d0 .09d0 .8d0 1))
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-adsr.2
      :md5 #(38 248 76 86 250 229 5 225 86 18 1 174 37 74 74 250))
  (edit-envelope *env-test* :adsr '(.2d0 .09d0 .8d0 1) :base 12)
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-dadsr.1
      :md5 #(54 21 246 223 0 162 133 169 167 111 125 202 74 148 53 204))
  (edit-envelope *env-test* :dadsr '(.75d0 .4d0 .1d0 .7d0 1.2d0))
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.6 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-breakpoint-env.1
      :md5 #(105 240 221 165 80 160 119 68 16 61 16 145 12 88 193 21))
  (vug-breakpoint-env-test '(0 0 1 1 2 0) -3 8 5))

(with-dsp-test (vug-breakpoint-env.2
      :md5 #(196 200 137 98 100 199 34 186 144 24 141 112 134 192 191 223))
  (vug-breakpoint-env-test '(0 0 1 1 10 0) -3 8 5))

(with-dsp-test (vug-breakpoint-env.3
      :md5 #(80 62 185 33 170 3 218 0 238 6 54 73 65 247 155 122))
  (vug-breakpoint-env-test '(0 0 1 1 10 0) -3 8 1)
  (loop for bp in '((0 0 1 1 2 0) (0 0 8 1 10 0) (0 0 1 1 2 .2d0 4 .8d0 7 0)
                    (0 0 1 .5d0 2 .2d0 3 .8d0 4 .1d0 5 1 8 0))
        for i from 1 do
          (at #[i s] #'vug-breakpoint-env-test bp -3 8 1)))

(defun ugen-breakpoint-env-test (bp-list length)
  (with-cleanup
    (let ((u (funcall
               (envelope* (breakpoints->env bp-list
                            :duration (* length *sample-duration*))
                          1 1 #'free))))
      (loop repeat length
            collect (progn
                      (funcall (ugen-perf-function u))
                      (round (smp-ref (ugen-return-pointer u) 0)))))))

(with-ugen-test (ugen-breakpoint-env.1)
    (ugen-breakpoint-env-test '(0 0 1 100 2 0) 10)
  (0 20 40 60 80 100 75 50 25 0))

(with-ugen-test (ugen-breakpoint-env.2)
    (ugen-breakpoint-env-test '(0 0 1 100 2 0) 13)
  (0 17 33 50 67 83 100 83 67 50 33 17 0))

(with-dsp-test (ramp-test.1
      :md5 #(255 169 119 69 127 35 240 42 227 232 100 47 39 238 159 41))
  (ramp-test-1 0 1 1.5)
  (at #[2 s] #'set-controls 1 :end 0 :dur 1.2)
  (at #[3.4 s] #'set-controls 1 :end .8 :dur .3)
  (at #[4 s] #'set-controls 1 :start 0 :end 1 :dur .5))

(with-dsp-test (expon-test.1
      :md5 #(3 14 6 50 1 201 11 118 113 59 91 237 74 33 189 175))
  (expon-test-1 0 1 1.5)
  (at #[2 s] #'set-controls 1 :end 0 :dur 1.2)
  (at #[3.4 s] #'set-controls 1 :end .8 :dur .3)
  (at #[4 s] #'set-controls 1 :start 0 :end 1 :dur .5))
