(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! performance-time-vug-variable-1 (x)
  (with-samples ((gate +sample-zero+))
    (setf gate x)
    (out (envelope (make-adsr .5f0 .1f0 .8f0 .7f0) gate))))

(dsp! let*-1 ()
  (with ((car .3f0)
         (cdr (list .2f0)))
    (declare (single-float car))
    (initialize
      (let* ((x car) ; Bug fixed: CAR interpreted as function.
             (y (car cdr))
             (a (+ x y)))
        (declare (single-float x y a))
        ;; Compiler note on 32-bit machines:
        ;; doing float to pointer coercion (cost 13).
        (reduce-warnings (setf cdr a))))
    (out (+ .2f0 car) cdr)))

(with-dsp-test (performance-time-vug-variable.1
      :md5 #(215 226 176 81 183 66 181 124 201 173 251 194 65 201 217 53))
  (performance-time-vug-variable-1 0 :id 123)
  (at #[1 s] #'set-control 123 'x 1)
  (at #[2 s] #'set-control 123 'x 0)
  (at #[3 s] #'set-control 123 'x 1)
  (at #[4 s] #'set-control 123 'x 0))

(with-dsp-test (let*.1 :channels 2
      :md5 #(2 137 205 126 140 66 41 71 73 191 58 153 177 159 173 31))
  (let*-1))
