(in-package :incudine-tests)

(dsp! reducible-var-1 (amp)
  (vuglet ((test (freq)
             ;; Precompute "(- +twopi+ 0)" during the simplification of
             ;; the VUG-VARIABLEs.
             (sin (wrap (* 10 (phasor freq 0)) 0 +twopi+))))
    (out (* amp (test 440)))))

(with-dsp-test (reducible-var.1
      :md5 #(130 182 107 207 33 216 214 78 203 110 12 176 35 62 93 168))
  (reducible-var-1 .8))

(deftest reduce-op.1
    (mapcar
      (lambda (form)
        (let ((res (vug::reduce-simple-op (car form) (cdr form))))
          (if (atom res) res (cons (car form) res))))
      '(;; +
        (+ 7) (+ a) (+ 0) (+ 0 0.0d0 0) (+ 1 2 3 4 5)
        (+ a 1) (+ 1 a) (+ a 1 b 2 3 c 4 5 6 d 7)
        (+ 0 1 0 a 0 2 0 0 b 0 0 0 3 0 c 0 0)
        ;; -
        (- 1) (- a) (- 0) (- a 0) (- a 0 b 0) (- 0 0.0d0 0) (- 0 0 a)
        (- a 1) (- 1 a) (- a 1 b 2 3 c 4 5 6 d 7) (- 1 1)
        (- 5 3 2 a -9 2 3 4 b 8 -8 c 2 -5 3)
        ;; /
        (/ 0 a) (/ a 2) (/ 2 a 1 1 2 1 1 1 4) (/ a) (/ 1) (/ 2)
        ;; *
        (* 7) (* a) (* a b 0 c d 1 2 3 e) (* a 1 b 1 c 1 1) (* 3 a)))
  (;; +
   7 A 0 0.0d0 15
   (+ A 1) (+ 1 A) (+ A 1 B 5 C 15 D 7)
   (+ 1 A 2 B 3 C)
   ;; -
   -1 (- A) 0 A (- A B) 0.0d0 (- A)
   (- A 1) (- 1 A) (- A 1 B 5 C 15 D 7) 0
   (- A B C)
   ;; /
   0 (/ A 2) (/ 2 A 1/2) (/ A) 1 1/2
   ;; *
   7 A 0 (* A B C) (* 3 A)))
