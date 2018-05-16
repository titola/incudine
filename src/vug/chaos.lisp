;;; Copyright (c) 2013-2018 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine.vug)

(defmacro chaos-interpolate ((output freq interpolation init-value-p)
                             &body body)
  `(interpolate (prog1 ,output ,@body)
                ,freq ,interpolation
                ,(and init-value-p (eq interpolation :cubic))))

;;; Chaotic ugens used in SuperCollider.

(define-vug-macro cusp (freq a b xinit &optional interpolation)
  "Cusp map chaotic generator with frequency FREQ.

The formula is

    x = a - b * sqrt(|x|)

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (cusp)
    `(vuglet ((,cusp (freq a b xinit)
                (with-samples ((x xinit))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf x (- a (* b (sqrt (abs x)))))))))
       (,cusp ,freq ,a ,b ,xinit))))

(define-vug-macro fb-sine (freq index-mult feedback phase-mult phase-add
                           xinit yinit &optional interpolation)
  "Feedback sine with chaotic phase indexing and frequency FREQ.

The formula is

    x = sin(index_mult * y + feedback * x)
    y = (phase_mult * y + phase_add) % 2pi

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (fb-sine)
    `(vuglet ((,fb-sine (freq index-mult feedback phase-mult phase-add
                         xinit yinit)
                (with-samples ((x xinit) (y yinit))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf x (sin (+ (* index-mult y) (* feedback x)))
                          y (wrap (+ (* phase-mult y) phase-add) 0 +twopi+))))))
       (,fb-sine ,freq ,index-mult ,feedback ,phase-mult ,phase-add ,xinit
                 ,yinit))))

;;; Gingerbreadman map chaotic generator; See Devaney, R. L.
;;; "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992.
(define-vug-macro gbman (freq xinit yinit &optional interpolation)
  "Gingerbreadman map chaotic generator with frequency FREQ.

The formula is

    x1 = x0
    x0 = 1 - y + |x0|
    y  = x1

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (gbman)
    `(vuglet ((,gbman (freq xinit yinit)
                (with-samples ((x0 xinit) (x1 0) (y yinit))
                  (chaos-interpolate (x0 freq ,interpolation t)
                    (setf x1 x0
                          x0 (+ (- 1 y) (abs x0))
                          y x1)))))
       (,gbman ,freq ,xinit ,yinit))))

(define-vug-macro henon (freq a b x0 x1 &optional interpolation)
  "Hénon map chaotic generator with frequency FREQ.

The formula is

    x = 1 - a*x0^2 + b*x1

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (henon)
    `(vuglet ((,henon (freq a b x0 x1)
                (with-samples ((xn x1) (xnm1 x0) (xnm2 x1))
                  (chaos-interpolate (xnm2 freq ,interpolation t)
                    (setf xn (+ (- 1.0 (* a xnm1 xnm1)) (* b xnm2)))
                    ;; Prevent instability
                    (nclip xn (sample -1.5) (sample 1.5))
                    (setf xnm2 xnm1 xnm1 xn)))))
       (,henon ,freq ,a ,b ,x0 ,x1))))

;;; Latoocarfian chaotic generator; see Clifford Pickover's book "Chaos
;;; In Wonderland", pag 26.
(define-vug-macro latoocarfian (freq a b c d xinit yinit
                                &optional interpolation)
  "Latoocarfian chaotic generator with frequency FREQ.

The formula is

    x1 = x0
    x0 = sin(b*y) + c*sin(b*x0)
    y  = sin(a*y) + d*sin(a*x1)

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (latoocarfian)
    `(vuglet ((,latoocarfian (freq a b c d xinit yinit)
                (with-samples ((x0 xinit) (x1 0) (y yinit))
                  (chaos-interpolate (x0 freq ,interpolation t)
                    (setf x1 x0
                          x0 (+ (sin (* y  b)) (* c (sin (* x0 b))))
                          y  (+ (sin (* x1 a)) (* d (sin (* y  a)))))))))
       (,latoocarfian ,freq ,a ,b ,c ,d ,xinit ,yinit))))

(define-vug-macro lin-cong (freq mult increment modulus xinit
                            &optional interpolation)
  "Linear congruential chaotic generator with frequency FREQ.

The formula is

    x = (mult * x + increment) % modulus

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (lin-cong)
    `(vuglet ((,lin-cong (freq mult increment modulus xinit)
                (with-samples ((x xinit)
                               (m modulus)
                               (scale (/ 2.0 m))
                               (xscaled (1- (* x scale))))
                  (chaos-interpolate (xscaled freq ,interpolation t)
                    (setf x (wrap (+ (* x mult) increment) +sample-zero+ m)
                          xscaled (1- (* x scale)))))))
       (,lin-cong ,freq ,mult ,increment ,modulus ,xinit))))

(define-vug lorenz (s r b integration-time xinit yinit zinit)
  "Lorenz chaotic generator with frequency FREQ.

The formula is

    x' = s*(y - x)
    y' = x*(r - z) - y
    z' = x*y - b*z

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default).

Return a frame with the three coordinates."
  (with-samples ((x xinit) (y yinit) (z zinit) x0 y0 z0)
    (setf x0 (* s (- y x))
          y0 (- (* r x) (* x z) y)
          z0 (- (* x y) (* b z)))
    (samples (incf x (* integration-time x0))
             (incf y (* integration-time y0))
             (incf z (* integration-time z0)))))

(define-vug-macro quad-map (freq a b c xinit &optional interpolation)
  "General quadratic map chaotic generator with frequency FREQ.

The formula is

    x = a*x^2 + b*x + c

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (quad-map)
    `(vuglet ((,quad-map (freq a b c xinit)
                (with-samples ((x xinit))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf x (+ (* a x x) (* b x) c))))))
       (,quad-map ,freq ,a ,b ,c ,xinit))))

(define-vug-macro standard-map (freq perturbation xinit yinit
                                &optional interpolation)
  "Standard map chaotic generator with frequency FREQ.

The formula is

    x = (x + y) % 2pi
    y = (y + perturbation * sin(x)) % 2pi

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default)."
  (with-gensyms (standard-map)
    `(vuglet ((,standard-map (freq perturbation xinit yinit)
                (with-samples ((xn xinit) (yn yinit) (x 0))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf yn (wrap (+ yn (* perturbation (sin xn)))
                                   +sample-zero+ +twopi+)
                          xn (wrap (+ xn yn) +sample-zero+ +twopi+)
                          x  (* (- xn pi) #.(/ 1.0 pi)))))))
       (,standard-map ,freq ,perturbation ,xinit ,yinit))))
