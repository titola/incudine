;;; Copyright (c) 2013-2015 Tito Latini
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

(in-package :incudine.gen)

(defmacro defwindow (name (c-array-var size-var &rest args) &body body)
  (multiple-value-bind (decl rest)
      (incudine.util::separate-declaration body)
    `(defun ,name ,args ,@decl
       (lambda (,c-array-var ,size-var)
         (declare (type foreign-pointer ,c-array-var)
                  (type positive-fixnum ,size-var))
         (locally (declare #.*standard-optimize-settings*)
           ,@rest)))))

(defmacro symmetric-loop ((var0 var1 count &optional (result nil)) &body body)
  (with-gensyms (half-count)
    `(do* ((,half-count (ash (1- ,count) -1))
           (,var0 0 (1+ ,var0))
           (,var1 (1- ,count) (1- ,var1)))
          ((> ,var0 ,half-count) ,result)
       (declare (type non-negative-fixnum ,var0 ,var1 ,half-count))
       ,@body)))

(defmacro symmetric-set (c-array index0 index1 value)
  `(setf (smp-ref ,c-array ,index0) ,value
         (smp-ref ,c-array ,index1) (smp-ref ,c-array ,index0)))

(defwindow bartlett (c-array size)
  (with-samples ((half-recip (/ (sample 2) (1- size))))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j (* i half-recip)))))

(defwindow blackman (c-array size)
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j
                     (+ (- 0.42 (* 0.5 (cos (the limited-sample phase))))
                        (* 0.08 (cos (the limited-sample (* 2 phase))))))
      (incf phase delta))))

(defwindow gaussian (c-array size &optional (beta 4.5))
  (declare (type alexandria:non-negative-real beta)
           #.*reduce-warnings*)
  (with-samples ((c (sample (* -0.5 beta beta)))
                 (k (/ (sample (- 1 size)) size))
                 (inc (/ (sample 2) size)))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j (exp (* c k k)))
      (incf k inc))))

(defwindow hamming (c-array size)
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j
                     (- 0.54 (* 0.46 (cos (the limited-sample phase)))))
      (incf phase delta))))

(defwindow hanning (c-array size)
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j
                     (* 0.5 (- 1.0 (cos (the limited-sample phase)))))
      (incf phase delta))))

(declaim (inline bessel-i0))
(defun bessel-i0 (beta)
  (cffi:foreign-funcall "gsl_sf_bessel_I0"
                        :double (coerce beta 'double-float)
                        :double))

(defwindow kaiser (c-array size &optional (beta 12))
  (declare (type alexandria:non-negative-real beta)
           #.*reduce-warnings*)
  (if (= size 1)
      (setf (smp-ref c-array 0) (sample 1))
      (with-samples ((bessel-i0-beta-r (sample (/ (bessel-i0 beta)))))
        (let* ((len (1- size))
               (c (/ (* 2 beta) len)))
          (symmetric-loop (i j size c-array)
            (symmetric-set c-array i j
                           (* (sample (bessel-i0 (* c (sqrt (* i (- len i))))))
                              bessel-i0-beta-r)))))))

(defwindow sinc (c-array size &optional (beta 1))
  (declare (type alexandria:non-negative-real beta)
           #.*reduce-warnings*)
  (with-samples ((delta (sample (/ (* +twopi+ beta) (1- size))))
                 (phase (sample (* beta (- pi)))))
    (flet ((%sinc (x)
             (/ (sin (the limited-sample x)) x)))
      (do* ((half-count (ash (1- size) -1))
            (i 0 (1+ i))
            (j (1- size) (1- j)))
           ((= i half-count)
            (setf (smp-ref c-array i)
                  (if (zerop phase) (sample 1) (%sinc phase)))
            (unless (= i j)
              (setf (smp-ref c-array j) (smp-ref c-array i)))
            c-array)
        (declare (type non-negative-fixnum i j half-count))
        (symmetric-set c-array i j (%sinc phase))
        (incf phase delta)))))

(defwindow dolph-chebyshev (c-array size &optional (attenuation 120))
  (declare (type real attenuation))
  (let* ((m (ash size -1))
         (n (* m 2)))
    (declare (type non-negative-fixnum m n))
    (with-samples* ((alpha (reduce-warnings (sample (/ attenuation 20))))
                    (beta (reduce-warnings
                            (cosh (/ (acosh (expt 10 alpha)) (1- n)))))
                    (sum 0)
                    (max 0)
                    (p 0))
      (dotimes (i n)
        (setf sum +sample-zero+)
        (setf p (sample (/ (* i +twopi+) n)))
        (loop for k below m
              for s of-type fixnum = 1 then (- s)
              do (incf sum (* s (cheb n (* beta (cos (the limited-sample
                                                       (/ (* pi k) n)))))
                              (cos (the limited-sample (* k p))))))
        (setf sum (- (/ sum (cheb n beta)) 0.5))
        (when (> sum max) (setf max sum))
        (setf (smp-ref c-array i) sum))
      (unless (zerop (- size n))
        (setf (smp-ref c-array n) +sample-zero+))
      (unless (or (zerop max) (= max 1))
        (setf max (/ max))
        (dotimes (i n)
          (setf #1=(smp-ref c-array i) (* #1# max))))
      c-array)))

(defwindow sine-window (c-array size)
  (with-samples ((winc (/ pi (1- size))))
    (symmetric-loop (i j size c-array)
      (symmetric-set c-array i j (sin (the limited-sample (* i winc)))))))
