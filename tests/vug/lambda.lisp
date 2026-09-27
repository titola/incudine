(in-package :incudine-tests)

;;; Bug fixed: the compilation fails if the first atom in lambda-body
;;; is a function name.
(dsp! lambda-body-test ()
  (funcall (lambda (min max) min (1+ max) (out max)) 0d0 1d0))

(dsp! lambda-nested-vug-expansion-1 ()
  (out (funcall
         (lambda ()
           (reson (noise-test 1) 1000 10)))))

(dsp! lambda-nested-vug-expansion-2 ()
  (funcall
    (lambda ()
      (out (reson (noise-test 1) 1000 10)))))

(with-dsp-test (lambda-body.1
      :md5 #(178 94 198 208 162 38 4 78 160 213 55 138 91 220 83 142))
  (lambda-body-test))

(with-dsp-test (lambda-vug-expansion.1
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-1))

(with-dsp-test (lambda-vug-expansion.2
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-2))
