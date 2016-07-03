;;; Copyright (c) 2013-2016 Tito Latini
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

;;; Cusp map chaotic generator.
;;;
;;; x = a - b * sqrt(|x|)
;;;
(define-vug-macro cusp (freq a b xinit &optional interpolation)
  (with-gensyms (cusp)
    `(vuglet ((,cusp (freq a b xinit)
                (with-samples ((x xinit))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf x (- a (* b (sqrt (abs x)))))))))
       (,cusp ,freq ,a ,b ,xinit))))

;;; Feedback sine with chaotic phase indexing.
;;;
;;; x = sin(im*y + fb*x)
;;; y = (a*y + c) % 2pi
;;;
(define-vug-macro fb-sine (freq index-mult feedback phase-mult phase-add
                           xinit yinit &optional interpolation)
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
;;;
;;; x1 = x0
;;; x0 = 1 - y + |x0|
;;; y  = x1
;;;
(define-vug-macro gbman (freq xinit yinit &optional interpolation)
  (with-gensyms (gbman)
    `(vuglet ((,gbman (freq xinit yinit)
                (with-samples ((x0 xinit) (x1 0) (y yinit))
                  (chaos-interpolate (x0 freq ,interpolation t)
                    (setf x1 x0
                          x0 (+ (- 1 y) (abs x0))
                          y x1)))))
       (,gbman ,freq ,xinit ,yinit))))

;;; Hénon map chaotic generator.
;;;
;;; x = 1 - a*x0^2 + b*x1
;;;
(define-vug-macro henon (freq a b x0 x1 &optional interpolation)
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
;;;
;;; x1 = x0
;;; x0 = sin(b*y) + c*sin(b*x0)
;;; y  = sin(a*y) + d*sin(a*x1)
;;;
(define-vug-macro latoocarfian (freq a b c d xinit yinit
                                &optional interpolation)
  (with-gensyms (latoocarfian)
    `(vuglet ((,latoocarfian (freq a b c d xinit yinit)
                (with-samples ((x0 xinit) (x1 0) (y yinit))
                  (chaos-interpolate (x0 freq ,interpolation t)
                    (setf x1 x0
                          x0 (+ (sin (* y  b)) (* c (sin (* x0 b))))
                          y  (+ (sin (* x1 a)) (* d (sin (* y  a)))))))))
       (,latoocarfian ,freq ,a ,b ,c ,d ,xinit ,yinit))))

;;; Linear congruential chaotic generator.
;;;
;;; x = (a*x + c) % m
;;;
(define-vug-macro lin-cong (freq mult increment modulus xinit
                            &optional interpolation)
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

;;; Lorenz chaotic generator.
;;;
;;; x' = s*(y - x)
;;; y' = x*(r - z) - y
;;; z' = x*y - b*z
;;;
;;; Return a frame with the three coordinates.
(define-vug lorenz (s r b integration-time xinit yinit zinit)
  (with-samples ((x xinit) (y yinit) (z zinit) x0 y0 z0)
    (setf x0 (* s (- y x))
          y0 (- (* r x) (* x z) y)
          z0 (- (* x y) (* b z)))
    (samples (incf x (* integration-time x0))
             (incf y (* integration-time y0))
             (incf z (* integration-time z0)))))

;;; General quadratic map chaotic generator.
;;;
;;; x = a*x^2 + b*x + c
;;;
(define-vug-macro quad-map (freq a b c xinit &optional interpolation)
  (with-gensyms (quad-map)
    `(vuglet ((,quad-map (freq a b c xinit)
                (with-samples ((x xinit))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf x (+ (* a x x) (* b x) c))))))
       (,quad-map ,freq ,a ,b ,c ,xinit))))

;;; Standard map chaotic generator.
;;;
;;; x = (x + y) % 2pi
;;; y = (y + k*sin(x)) % 2pi
;;;
(define-vug-macro standard-map (freq perturbation xinit yinit
                                &optional interpolation)
  (with-gensyms (standard-map)
    `(vuglet ((,standard-map (freq perturbation xinit yinit)
                (with-samples ((xn xinit) (yn yinit) (x 0))
                  (chaos-interpolate (x freq ,interpolation t)
                    (setf yn (wrap (+ yn (* perturbation (sin xn)))
                                   +sample-zero+ +twopi+)
                          xn (wrap (+ xn yn) +sample-zero+ +twopi+)
                          x  (* (- xn pi) #.(/ 1.0 pi)))))))
       (,standard-map ,freq ,perturbation ,xinit ,yinit))))
