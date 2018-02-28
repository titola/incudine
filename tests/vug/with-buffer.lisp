(in-package :incudine-tests)

(dsp! vug-with-buffer-test-1 ()
  (with-buffer (b 256 :fill-function (gen:partials '(1)))
    (with ((i 0))
      (declare (fixnum i))
      (out (buffer-value b i))
      (setf i (logand (1+ i) 255)))))

(dsp! vug-with-buffer-test-2 ()
  (with-buffers ((b 256 :fill-function (gen:partials '(1)))
                 (w 256 :fill-function (gen:hanning)))
    (with ((i 0))
      (declare (fixnum i))
      (out (* (buffer-value b i) (buffer-value w i)))
      (setf i (logand (1+ i) 255)))))

(with-dsp-test (vug-with-buffer.1
      :md5 #(141 149 119 98 100 86 132 42 21 179 23 74 127 227 8 73))
  (vug-with-buffer-test-1))

(with-dsp-test (vug-with-buffer.2
      :md5 #(235 100 74 29 117 52 217 177 14 169 193 166 139 209 210 157))
  (vug-with-buffer-test-2))
