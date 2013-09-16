;;; Copyright (c) 2013 Tito Latini
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
  (with-gensyms (x)
    (with-coerce-arguments (xinit)
      `(with-samples ((,x ,xinit))
         (chaos-interpolate (,x ,freq ,interpolation t)
             (setf ,x (- ,a (* ,b (sqrt (abs ,x))))))))))

;;; Feedback sine with chaotic phase indexing.
;;;
;;; x = sin(im*y + fb*x)
;;; y = (a*y + c) % 2pi
;;;
(define-vug-macro fb-sine (freq index-mult feedback phase-mult phase-add
                           xinit yinit &optional interpolation)
  (with-gensyms (x y)
    (with-coerce-arguments (xinit yinit)
      `(with-samples ((,x ,xinit) (,y ,yinit))
         (chaos-interpolate (,x ,freq ,interpolation t)
             (setf ,x (sin (+ (* ,index-mult ,y) (* ,feedback ,x)))
                   ,y (wrap (+ (* ,phase-mult ,y) ,phase-add) 0 +twopi+)))))))

;;; Gingerbreadman map chaotic generator; See Devaney, R. L.
;;; "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992.
;;;
;;; x1 = x0
;;; x0 = 1 - y + |x0|
;;; y  = x1
;;;
(define-vug-macro gbman (freq xinit yinit &optional interpolation)
  (with-gensyms (x0 x1 y)
    (with-coerce-arguments (xinit yinit)
      `(with-samples ((,x0 ,xinit) (,x1 0.0d0) (,y ,yinit))
         (chaos-interpolate (,x0 ,freq ,interpolation t)
             (setf ,x1 ,x0
                   ,x0 (+ (- 1 ,y) (abs ,x0))
                   ,y ,x1))))))

;;; Hénon map chaotic generator.
;;;
;;; x = 1 - a*x0^2 + b*x1
;;;
(define-vug-macro henon (freq a b x0 x1 &optional interpolation)
  (with-gensyms (xn xnm1 xnm2)
    (with-coerce-arguments (x0 x1)
      `(with-samples ((,xn ,x1) (,xnm1 ,x0) (,xnm2 ,x1))
         (chaos-interpolate (,xnm2 ,freq ,interpolation t)
           (setf ,xn (+ (- 1.0 (* ,a ,xnm1 ,xnm1)) (* ,b ,xnm2)))
           ;; Prevent instability
           (nclip ,xn (sample -1.5) (sample 1.5))
           (setf ,xnm2 ,xnm1 ,xnm1 ,xn))))))

;;; Latoocarfian chaotic generator; see Clifford Pickover's book "Chaos
;;; In Wonderland", pag 26.
;;;
;;; x1 = x0
;;; x0 = sin(b*y) + c*sin(b*x0)
;;; y  = sin(a*y) + d*sin(a*x1)
;;;
(define-vug-macro latoocarfian (freq a b c d xinit yinit
                                &optional interpolation)
  (with-gensyms (x0 x1 y)
    (with-coerce-arguments (xinit yinit)
      `(with-samples ((,x0 ,xinit) (,x1 0.0d0) (,y ,yinit))
         (chaos-interpolate (,x0 ,freq ,interpolation t)
           (setf ,x1 ,x0
                 ,x0 (+ (sin (* ,y  ,b)) (* ,c (sin (* ,x0 ,b))))
                 ,y  (+ (sin (* ,x1 ,a)) (* ,d (sin (* ,y  ,a))))))))))

;;; Linear congruential chaotic generator.
;;;
;;; x = (a*x + c) % m
;;;
(define-vug-macro lin-cong (freq mult increment modulus xinit
                            &optional interpolation)
  (with-gensyms (x xscaled scale m)
    (with-coerce-arguments (xinit)
      `(with-samples ((,x ,xinit)
                      (,m ,modulus)
                      (,scale (/ 2.0 ,m))
                      (,xscaled (- (* ,x ,scale) 1.0)))
         (chaos-interpolate (,xscaled ,freq ,interpolation t)
           (setf ,x (wrap (+ (* ,x ,mult) ,increment) +sample-zero+ ,m)
                 ,xscaled (- (* ,x ,scale) 1.0)))))))

;;; Lorenz chaotic generator.
;;;
;;; x' = s*(y - x)
;;; y' = x*(r - z) - y
;;; z' = x*y - b*z
;;;
;;; Return a frame with the three coordinates.
(define-vug-macro lorenz (s r b integration-time xinit yinit zinit)
  (with-gensyms (x y z x0 y0 z0 frm)
    (with-coerce-arguments (xinit yinit zinit)
      `(with ((,x ,xinit)
              (,y ,yinit)
              (,z ,zinit)
              (,x0 0.0d0)
              (,y0 0.0d0)
              (,z0 0.0d0)
              (,frm (make-frame 3)))
         (declare (type sample ,x ,y ,z ,x0 ,y0 ,z0) (type frame ,frm))
         (setf ,x0 (* ,s (- ,y ,x))
               ,y0 (- (* ,r ,x) (* ,x ,z) ,y)
               ,z0 (- (* ,x ,y) (* ,b ,z)))
         (incf ,x (* ,integration-time ,x0))
         (incf ,y (* ,integration-time ,y0))
         (incf ,z (* ,integration-time ,z0))
         (setf (frame-ref ,frm 0) ,x
               (frame-ref ,frm 1) ,y
               (frame-ref ,frm 2) ,z)
         ,frm))))

;;; General quadratic map chaotic generator.
;;;
;;; x = a*x^2 + b*x + c
;;;
(define-vug-macro quad-map (freq a b c xinit &optional interpolation)
  (with-gensyms (x)
    (with-coerce-arguments (xinit)
      `(with-samples ((,x ,xinit))
         (chaos-interpolate (,x ,freq ,interpolation t)
           (setf ,x (+ (* ,a ,x ,x) (* ,b ,x) ,c)))))))

;;; Standard map chaotic generator.
;;;
;;; x = (x + y) % 2pi
;;; y = (y + k*sin(x)) % 2pi
;;;
(define-vug-macro standard-map (freq perturbation xinit yinit
                                &optional interpolation)
  (with-gensyms (x xn yn)
    (with-coerce-arguments (xinit yinit)
      `(with-samples ((,xn ,xinit) (,yn ,yinit) (,x 0.0d0))
         (chaos-interpolate (,x ,freq ,interpolation t)
           (setf ,yn (wrap (+ ,yn (* ,perturbation (sin ,xn)))
                           +sample-zero+ +twopi+)
                 ,xn (wrap (+ ,xn ,yn) +sample-zero+ +twopi+)
                 ,x  (* (- ,xn pi) #.(/ 1.0 pi))))))))
