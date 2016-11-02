(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(let ((x 0))
  (declare (fixnum x))
  (defun noise-func-init (node)
    (setf x 12345)
    node)
  (defun noise-func-test ()
    (declare (optimize speed (safety 0)))
    (setf x (logand (+ 17711 (* x 9227465)) #xffffff))
    (incf (audio-out 0) (* (- x #x7fffff) 1d-7))
    (values)))

(defun simple-lp-init (node)
  (setf (bus 0) +sample-zero+)
  node)

(defun simple-lp ()
  (declare (optimize speed (safety 0)))
  (let ((in (audio-out 0)))
    (setf (audio-out 0) (* 0.5 (+ (bus 0) in)))
    (setf (bus 0) in)
    (values)))

(let ((amp 0)
      (current-node nil))
  (declare (type fixnum amp) (type (or node null) current-node))
  (defun sinosc-func-init (node)
    (setf amp 0)
    (setf current-node node))
  (defun sinosc-func-test ()
    (declare (optimize speed (safety 0)))
    (out (* amp 2d-10 amp
            (sin (the maybe-limited-sample
                   (* +twopi+ 441 *sample-duration* (now))))))
    (if (>= amp (sample->fixnum *sample-rate*))
        (reinit current-node)
        (incf amp))
    (values)))

(with-dsp-test (play.1
      :md5 #(143 213 36 78 17 182 59 193 47 205 59 10 11 156 173 191))
  (play #'noise-func-test :init-function #'noise-func-init :id 1)
  (at #[2.5 s] #'play #'simple-lp :init-function #'simple-lp-init :after 1
      :free-hook (list (lambda (n) (setf (bus 0) +sample-zero+) n))))

(with-dsp-test (play.2
      :md5 #(91 24 13 194 96 17 163 106 46 195 65 186 35 141 3 29))
  (play #'sinosc-func-test :init-function #'sinosc-func-init :id 1)
  (at #[5/2 s] #'reinit 1)
  (at #[4 s] #'reinit 1))

(with-dsp-test (play.3
      :md5 #(140 62 13 132 251 126 184 37 56 248 112 208 0 196 241 192))
  (play #'sinosc-func-test :init-function #'sinosc-func-init :id 1)
  (at #[1 s] #'play #'noise-func-test
      :init-function #'noise-func-init
      :replace 1)
  (at #[2 s] #'play #'sinosc-func-test
      :init-function #'sinosc-func-init
      :replace 1)
  (at #[3 s] #'play #'noise-func-test
      :init-function #'noise-func-init
      :replace 1)
  (at #[3.2 s] #'play #'simple-lp
      :init-function #'simple-lp-init
      :after 1
      :id 2
      :free-hook (list (lambda (n) (setf (bus 0) +sample-zero+) n)))
  (at #[3.3 s] #'play #'sinosc-func-test
      :init-function #'sinosc-func-init
      :tail 0
      :id 3)
  (at #[3.9 s] #'free 1)
  (at #[4 s] #'play #'noise-func-test
      :init-function #'noise-func-init
      :replace 3)
  (at #[4.2 s] #'move 3 :head 0))
