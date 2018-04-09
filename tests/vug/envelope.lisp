(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(defvar *env-test* (make-envelope '(0 1 0) '(.5 .5)))

(pushnew (lambda ()
           (edit-envelope *env-test* '(0 1 0) '(.5 .5) :curve :lin
                          :loop-node -1 :release-node -1))
         *test-hook*)

(dsp! vug-envelope-test ((env envelope) gain gate dur)
  (out (* (db->lin gain) (envelope env gate dur #'free))))

(dsp! vug-breakpoint-env-test ((bp cons) gain (base real) dur)
  (out (* (db->lin gain)
          (envelope (breakpoints->env bp :base base :duration 1)
                    1 dur #'free))))

(with-dsp-test (vug-envelope.1
      :md5 #(27 133 61 38 244 95 113 211 23 61 48 224 246 123 132 13))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.2
      :md5 #(21 244 178 46 77 71 55 119 171 84 13 146 128 30 157 254))
  (setf (envelope-curve *env-test* 1) :exp)
  (setf (envelope-curve *env-test* 2) :linear)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.3
      :md5 #(223 218 116 247 110 167 67 164 252 163 164 182 102 140 220 37))
  (setf (envelope-curve *env-test* 1) 4)
  (setf (envelope-curve *env-test* 2) -4)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.4
      :md5 #(67 12 215 32 127 93 217 151 206 235 251 142 116 77 162 47))
  (set-envelope-base *env-test* 7)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.5
      :md5 #(183 151 50 218 1 36 206 18 152 27 14 64 166 54 86 209))
  (edit-envelope *env-test* '(0 1 0) '(.2 .8) :curve (list :cub :sqr))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.6
      :md5 #(31 49 109 6 210 163 192 240 184 143 49 19 71 122 45 55))
  (edit-envelope *env-test* '(0 1 .3 .15) '(.2 .3 .5) :base .2)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-linen.1
      :md5 #(245 86 98 176 11 207 232 64 98 15 55 46 228 176 61 181))
  (edit-envelope *env-test* :linen '(.3 3.7 1))
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-linen.2
      :md5 #(100 4 192 51 198 96 130 216 12 146 14 121 211 114 7 126))
  (edit-envelope *env-test* :linen '(.3 3.7 1) :curve :sin)
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-perc.1
      :md5 #(23 14 194 147 4 126 127 179 133 61 139 190 77 61 70 9))
  (edit-envelope *env-test* :perc '(.001 1))
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-cutoff.1
      :md5 #(66 253 57 218 0 39 148 123 54 52 223 214 69 220 5 116))
  (edit-envelope *env-test* :cutoff 2)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-cutoff.2
      :md5 #(4 134 209 136 100 169 81 65 24 14 242 247 130 56 127 3))
  (edit-envelope *env-test* :cutoff 2 :curve 8)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-asr.1
      :md5 #(209 61 1 41 4 200 106 1 64 119 25 84 228 170 47 131))
  (edit-envelope *env-test* :asr '(1 1 1.5))
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[3 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-adsr.1
      :md5 #(60 250 238 0 15 1 113 169 120 24 233 159 35 39 245 69))
  (edit-envelope *env-test* :adsr '(.2 .09 .8 1))
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-adsr.2
      :md5 #(36 242 35 85 185 166 32 133 237 8 55 153 157 103 132 3))
  (edit-envelope *env-test* :adsr '(.2 .09 .8 1) :base 12)
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-dadsr.1
      :md5 #(114 95 25 30 207 71 2 217 136 255 187 232 126 22 167 118))
  (edit-envelope *env-test* :dadsr '(.75 .4 .1 .7 1.2))
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.6 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-breakpoint-env.1
      :md5 #(48 252 231 245 131 115 75 138 37 106 16 48 210 134 212 142))
  (vug-breakpoint-env-test '(0 0 1 1 2 0) -3 8 5))

(with-dsp-test (vug-breakpoint-env.2
      :md5 #(70 89 202 143 251 153 231 26 242 79 167 94 32 220 232 24))
  (vug-breakpoint-env-test '(0 0 1 1 10 0) -3 8 5))

(with-dsp-test (vug-breakpoint-env.3
      :md5 #(8 50 249 229 251 222 164 57 30 165 32 9 27 33 51 164))
  (vug-breakpoint-env-test '(0 0 1 1 10 0) -3 8 1)
  (loop for bp in '((0 0 1 1 2 0) (0 0 8 1 10 0) (0 0 1 1 2 .2 4 .8 7 0)
                    (0 0 1 .5 2 .2 3 .8 4 .1 5 1 8 0))
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
