(in-package :incudine-tests)

;;; Bug fixed: the compilation fails if the first atom in progn-form,
;;; let-form and lambda-body is a function name.
(dsp! lambda-body-test ()
  (let (I quote if (x 1.5))
    If I quote
    (let* (k (y (if k 987654 3)))
      Quote
      (funcall (lambda (min max) min (progn max x y) (out (- y x max))) 0d0 1d0))))

(dsp! lambda-nested-vug-expansion-1 ()
  (out (funcall
         (lambda ()
           (reson (noise-test 1) 1000 10)))))

(dsp! lambda-nested-vug-expansion-2 ()
  (funcall
    (lambda ()
      (out (reson (noise-test 1) 1000 10)))))

(with-dsp-test (lambda-body.1
      :md5 #(161 205 76 188 210 243 228 100 239 189 253 78 70 210 30 230))
  (lambda-body-test))

(with-dsp-test (lambda-vug-expansion.1
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-1))

(with-dsp-test (lambda-vug-expansion.2
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-2))
