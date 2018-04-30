;;; Copyright (c) 2017-2018 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:with-cleanup
     incudine:circular-shift
     incudine:freq-breakpoints->env
     incudine.analysis:rectangular-window
     incudine.analysis:make-ifft
     incudine.analysis:analysis-input-buffer
     incudine.analysis:compute-ifft)))

(defun fir (breakpoints &key (sample-rate *sample-rate*) (base 1.0) curve
            (normalize-p t))
  "Return a function called to fill a foreign array with the FIR
filter coefficients obtained from a sequence of break-point pairs
interpreted as frequency response.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns three values: the foreign
array, the scale factor to normalize the samples and the boolean
NORMALIZE-P to specify whether the normalization is necessary.

SAMPLE-RATE defaults to *sample-rate*.

CURVE sets the shape of the segments of the frequency response; the
possible values are :STEP, :LIN or :LINEAR, :EXP or :EXPONENTIAL, :SIN
or :SINE, :WEL or :WELCH, :SQR or :SQUARE, :CUB or :CUBIC, a number
that represents the curvature value between two levels for all the
segments or a list of the prior values to specify the curvature values
for each segment. CURVE is ignored if BASE is non-NIL.

BASE (1 by default) is e^k and the curvature depends on the highest
and lowest levels."
  (declare (type list breakpoints) (type real sample-rate base)
           (type boolean normalize-p))
  (lambda (foreign-array size)
    (declare (type foreign-pointer foreign-array)
             (type non-negative-fixnum size))
    (with-cleanup
      (let ((ifft (make-ifft (the non-negative-fixnum (* size 2))
                             :window-function #'rectangular-window)))
        (declare #.*standard-optimize-settings* #.*reduce-warnings*)
        ;; If BREAKPOINTS is NIL, the content of the passed FOREIGN-ARRAY
        ;; is the frequency response.
        (when breakpoints
          (funcall (gen:envelope (freq-breakpoints->env breakpoints
                                   :base base
                                   :curve curve
                                   :freq-max (* sample-rate .5)))
                   foreign-array size))
        (loop for i of-type non-negative-fixnum below size
              for j of-type non-negative-fixnum = (* i 2) do
                (setf (smp-ref (analysis-input-buffer ifft) j)
                      (smp-ref foreign-array i))
                (setf (smp-ref (analysis-input-buffer ifft) (1+ j))
                      +sample-zero+))
        (funcall (gen:analysis
                   (circular-shift (compute-ifft ifft nil t) (ash size -1)))
                 foreign-array size)
        (with-samples ((tmp 0) (max 0))
          (dotimes (i size)
            (setf tmp (abs (smp-ref foreign-array i)))
            (when (> tmp max) (setf max tmp)))
          (values foreign-array (/ max) normalize-p))))))
