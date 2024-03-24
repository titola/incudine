(in-package :incudine-tests)

(dsp! lambda-nested-vug-expansion-1 ()
  (out (funcall
         (lambda ()
           (reson (noise-test 1) 1000 10)))))

(dsp! lambda-nested-vug-expansion-2 ()
  (funcall
    (lambda ()
      (out (reson (noise-test 1) 1000 10)))))

(with-dsp-test (lambda-vug-expansion.1
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-1))

(with-dsp-test (lambda-vug-expansion.2
      :md5 #(4 133 71 10 87 203 65 24 111 16 125 177 233 154 128 66))
  (lambda-nested-vug-expansion-2))
