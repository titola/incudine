(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! initialize-test-1 (input)
  (with-samples ((x 0))
    (initialize (setf x input))
    (out x)))

(dsp! initialize-test-2 (input)
  (with-samples ((x input))
    (initialize (setf x (sample 0.5)))
    (out x)))

(with-dsp-test (initialize.1
      :md5 #(161 205 76 188 210 243 228 100 239 189 253 78 70 210 30 230))
  (initialize-test-1 .5 :id 123)
  ;; The output is always 0.5
  (at #[1 sec] #'set-control 123 :input .1)
  (at #[2 sec] #'set-control 123 :input .2)
  (at #[3 sec] #'set-control 123 :input .3)
  (at #[4 sec] #'set-control 123 :input .4))

(with-dsp-test (initialize.2
      :md5 #(58 59 186 56 169 81 3 112 235 79 166 101 121 240 58 175))
  (initialize-test-2 .5 :id 123)
  (at #[1 sec] #'set-control 123 :input .1)
  (at #[2 sec] #'set-control 123 :input .2)
  (at #[3 sec] #'set-control 123 :input .3)
  (at #[4 sec] #'set-control 123 :input .4))
