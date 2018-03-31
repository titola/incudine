(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! performance-time-vug-variable-1 (x)
  (with-samples ((gate +sample-zero+))
    (setf gate x)
    (out (envelope (make-adsr .5 .1 .8 .7) gate))))

(with-dsp-test (performance-time-vug-variable.1
      :md5 #(215 226 176 81 183 66 181 124 201 173 251 194 65 201 217 53))
  (performance-time-vug-variable-1 0 :id 123)
  (at #[1 s] #'set-control 123 'x 1)
  (at #[2 s] #'set-control 123 'x 0)
  (at #[3 s] #'set-control 123 'x 1)
  (at #[4 s] #'set-control 123 'x 0))
