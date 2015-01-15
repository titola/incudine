(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! delete-var-test-1 ((buf buffer) amp)
  (with-samples ((rate 1.0))
    (setf rate (sample (if (> amp 0.5) 100 200)))
    ;;; The PHASOR's rate depends on the VUG-PARAMETER BUF, therefore
    ;;; the related VUG-VARIABLE is to preserve.
    (out (* amp (buffer-play buf rate 0 t #'identity)))))

(with-dsp-test (delete-var.1
      :md5 #(53 186 250 177 0 178 241 139 224 231 126 34 95 170 167 51))
  (delete-var-test-1 *sine-table* .3 :id 123)
  (at #[3 s] #'set-control 123 :amp .55))
