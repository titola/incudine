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

(in-package :incudine.gen)

(defmacro defwindow (name (foreign-array-var size-var &rest args) &body body)
  "Define a new GEN routine named NAME with lambda-list ARGS to
generate window functions.

The variables FOREIGN-ARRAY-VAR and SIZE-VAR are bound to the window
function arguments.

Example:

    (gen:defwindow hanning (foreign-array size)
      (with-samples ((delta (/ +twopi+ (1- size)))
                     (phase +sample-zero+))
        (gen:symmetric-loop (i j size foreign-array)
          (gen:symmetric-set foreign-array i j
            (* 0.5 (- 1.0 (cos (the limited-sample phase)))))
          (incf phase delta))))"
  (multiple-value-bind (decl rest)
      (incudine.util::separate-declaration body)
    (let ((doc (and (stringp (car rest)) (car rest))))
      `(defun ,name ,args ,@(and doc `(,doc)) ,@decl
         (lambda (,foreign-array-var ,size-var)
           (declare (type foreign-pointer ,foreign-array-var)
                    (type positive-fixnum ,size-var))
           (incudine-optimize
             ,@(if doc (cdr rest) rest)))))))

(defmacro symmetric-loop ((var0 var1 count &optional (result nil)) &body body)
  "Iterate over the integer from 0 up to but not including the half of COUNT,
execute BODY with VAR0 bound to each integer and VAR1 bound to the integers
from COUNT minus 1 down to the half of COUNT, then RESULT form is evaluated.

Example:

    (gen:symmetric-loop (i j 8) (princ (list i j)))
    ;; => (0 7)(1 6)(2 5)(3 4)"
  (with-gensyms (half-count)
    `(do* ((,half-count (ash (1- ,count) -1))
           (,var0 0 (1+ ,var0))
           (,var1 (1- ,count) (1- ,var1)))
          ((> ,var0 ,half-count) ,result)
       (declare (type non-negative-fixnum ,var0 ,var1 ,half-count))
       ,@body)))

(defmacro symmetric-set (foreign-array index0 index1 value)
  "Companion of SYMMETRIC-LOOP, set the symmetric elements of
FOREIGN-ARRAY specified by INDEX0 and INDEX1 to VALUE."
  `(setf (smp-ref ,foreign-array ,index0) ,value
         (smp-ref ,foreign-array ,index1) (smp-ref ,foreign-array ,index0)))

(defwindow bartlett (foreign-array size)
  "Return a function called to fill a foreign array with a Bartlett window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((half-recip (/ (sample 2) (1- size))))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j (* i half-recip)))))

(defwindow blackman (foreign-array size)
  "Return a function called to fill a foreign array with a Blackman window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.42 (* 0.5 (cos (the limited-sample phase))))
                        (* 0.08 (cos (the limited-sample (* 2 phase))))))
      (incf phase delta))))

(defwindow gaussian (foreign-array size &optional (beta 4.5))
  "Return a function called to fill a foreign array with a Gaussian window
using BETA (4.5 by default) as the window parameter.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (declare (type alexandria:non-negative-real beta)
           #.*reduce-warnings*)
  (with-samples ((c (sample (* -0.5 beta beta)))
                 (k (/ (sample (- 1 size)) size))
                 (inc (/ (sample 2) size)))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j (exp (* c k k)))
      (incf k inc))))

(defwindow hamming (foreign-array size)
  "Return a function called to fill a foreign array with a Hamming window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (- 0.54 (* 0.46 (cos (the limited-sample phase)))))
      (incf phase delta))))

(defwindow hanning (foreign-array size)
  "Return a function called to fill a foreign array with a Hanning window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (* 0.5 (- 1.0 (cos (the limited-sample phase)))))
      (incf phase delta))))

(declaim (inline bessel-i0))
(defun bessel-i0 (beta)
  (cffi:foreign-funcall "gsl_sf_bessel_I0"
                        :double (coerce beta 'double-float)
                        :double))

(defwindow kaiser (foreign-array size &optional (beta 12))
  "Return a function called to fill a foreign array with a Kaiser window
using BETA (12 by default) as the window parameter.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (declare (type alexandria:non-negative-real beta)
           #.*reduce-warnings*)
  (if (= size 1)
      (setf (smp-ref foreign-array 0) (sample 1))
      (with-samples ((bessel-i0-beta-r (sample (/ (bessel-i0 beta)))))
        (let* ((len (1- size))
               (c (/ (* 2 beta) len)))
          (symmetric-loop (i j size foreign-array)
            (symmetric-set foreign-array i j
                           (* (sample (bessel-i0 (* c (sqrt (* i (- len i))))))
                              bessel-i0-beta-r)))))))

(defwindow sinc (foreign-array size &optional (beta 1))
  "Return a function called to fill a foreign array with a sinc window
using BETA (1 by default) as the window parameter.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
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
            (setf (smp-ref foreign-array i)
                  (if (zerop phase) (sample 1) (%sinc phase)))
            (unless (= i j)
              (setf (smp-ref foreign-array j) (smp-ref foreign-array i)))
            foreign-array)
        (declare (type non-negative-fixnum i j half-count))
        (symmetric-set foreign-array i j (%sinc phase))
        (incf phase delta)))))

(defwindow dolph-chebyshev (foreign-array size &optional (attenuation 120))
  "Return a function called to fill a foreign array with a Dolph-Chebyshev
window using ATTENUATION (120 dB by default) as the window parameter.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
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
        (setf (smp-ref foreign-array i) sum))
      (unless (zerop (- size n))
        (setf (smp-ref foreign-array n) +sample-zero+))
      (unless (or (zerop max) (= max 1))
        (setf max (/ max))
        (dotimes (i n)
          (setf #1=(smp-ref foreign-array i) (* #1# max))))
      foreign-array)))

(defwindow sine-window (foreign-array size)
  "Return a function called to fill a foreign array with a sine window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((winc (/ pi (1- size))))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j (sin (the limited-sample (* i winc)))))))
