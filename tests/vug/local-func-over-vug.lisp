(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! flet-over-vug-1 (freq amp f-percent a-percent)
  (with-samples ((m0 (flet ((scaler (x)
                              (* x 1d-2)))
                       (scaler f-percent)))
                 (m1 (* a-percent 1d-2)))
    (flet ((prova (fscale ascale)
             (declare (type sample fscale ascale))
             (sine (* fscale freq) (* ascale amp))))
      (out (prova m0 m1)))))

(dsp! labels-over-vug-1 (freq amp f-percent a-percent)
  (with-samples ((m0 (labels ((scaler (x) (div100 x))
                              (div100 (x) (* x 1d-2)))
                       (scaler f-percent)))
                 (m1 (* a-percent 1d-2)))
    (labels ((prove (fscale ascale)
               (+ (prova (* fscale 3/2) (* ascale .8d0))
                  (sine (* fscale freq) (* ascale amp))))
             (prova (fscale ascale)
               (declare (type sample fscale ascale))
               (sine (* fscale freq) (* ascale amp))))
      (out (prove m0 m1)))))

(defun function-on-local-name-test () 54321)

(dsp! function-on-local-name ()
  (initialize
    (flet ((function-on-local-name-test () 12345))
      (unless (= (funcall #'function-on-local-name-test) 12345)
        (error "BUG!! FUNCTION on a local function name."))))
  (out +sample-zero+))

(with-dsp-test (flet-over-vug.1
      :md5 #(211 78 65 19 12 203 20 240 201 69 126 145 221 161 111 130))
  (flet-over-vug-1 4400 1 10 30 :id 1)
  (at #[3 s] #'set-controls 1 :freq 3300 :amp .5)
  (at #[4 s] #'set-controls 1 :f-percent 20 :a-percent 40))

(with-dsp-test (labels-over-vug.1
      :md5 #(197 202 227 171 175 207 213 143 241 152 138 183 16 130 189 52))
  (labels-over-vug-1 4400 1 10 30 :id 1)
  (at #[3 s] #'set-controls 1 :freq 3300 :amp .5)
  (at #[4 s] #'set-controls 1 :f-percent 20 :a-percent 40))

(deftest function-on-local-name.1
    (progn
      (setf (buffer-value *buffer-test-c1* 100) (sample 1))
      (bounce-to-buffer (*buffer-test-c1*) (function-on-local-name))
      (buffer-value *buffer-test-c1* 100))
  #.+sample-zero+)
