;;; Copyright (c) 2013-2023 Tito Latini
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

(declaim (inline partial-ref))
(defun partial-ref (partial-number amp phase dc index size tmp-var)
  (declare (type fixnum index size)
           (type real partial-number amp phase dc))
  (setf (smp-ref tmp-var)
        (+ dc (* amp (sin (the limited-sample
                            (+ (/ (* +twopi+ index partial-number) size)
                               (* +twopi+ phase))))))))

(defun complete-partial-list (lst)
  (declare (type list lst))
  (do* ((i 1 (1+ i))
        (par-list lst (cdr par-list))
        (par (car par-list) (car par-list))
        (acc nil))
       ((null par) (nreverse acc))
    (declare (type list par-list acc) (type non-negative-fixnum i)
             (type (or real list) par))
    (if (numberp par)
        (if (minusp par)
            (push `(,i ,(- par) ,(sample 0.5) ,+sample-zero+) acc)
            (push `(,i ,par ,+sample-zero+ ,+sample-zero+) acc))
        (handler-case
            (destructuring-bind (n &optional (amp (sample 1))
                                 (phase +sample-zero+ phase-p)
                                 (os +sample-zero+))
                par
              (declare (type (real 0) n) (type real amp phase os))
              (push (list n (abs amp)
                          (if (or (>= amp 0) phase-p)
                              phase
                              (sample .5))
                          os)
                    acc)
              (setf i (floor n)))
          (error ()
            (error 'incudine:incudine-memory-fault-error
                   :format-control "Malformed list of partials:~%~A in ~A"
                   :format-arguments (list par lst)))))))

;;; Inspired by GEN09, GEN10 and GEN19 of Csound.
(defun partials (list &key (periodic-p t) (normalize-p t))
  "Return a function called to fill a foreign array with a composite
waveform made up of weighted sums of sinusoids. If PERIODIC-P is T (default),
the result is a cycle of a periodic waveform. LIST is a list where each element
is one of the following:

- value: relative strength of the partial. A negative value implies a
  phase inversion. The number of the partial is the position in the list
  plus a possible offset introduced by a previous list (partial-number strength
  [phase] [dc]). Example:

      (with-buffers
        ((b0 16 :fill-function (gen:partials '(1 0 .5 0 0 0 .2 .1)))
         (b1 16 :fill-function (gen:partials '(1 0 .5 (7 .2) .1))))
        (equal (buffer->list b0) (buffer->list b1)))
      ;; => T

- list (partial-number strength): STRENGTH is the strength of the partial
  PARTIAL-NUMBER (not necessarily an integer value). A negative STRENGTH
  value implies a phase inversion.

- list (partial-number strength phase): PHASE is the initial phase of the
  partial. It is a multiplier for +TWOPI+.

- list (partial-number strength phase dc): DC is the DC offset of the partial.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns three values: the foreign
array, the scale factor to normalize the samples and the boolean
NORMALIZE-P to specify whether the normalization is necessary.
"
  (declare (type list list) (type boolean periodic-p normalize-p))
  (let ((pl (complete-partial-list list)))
    (declare (type list pl))
    (lambda (foreign-pointer size)
      (declare (type foreign-pointer foreign-pointer)
               (type non-negative-fixnum size))
      (let ((size (if periodic-p size (1- size))))
        (declare (type non-negative-fixnum size)
                 #.*standard-optimize-settings* #.*reduce-warnings*)
        (with-samples (abs-value (max 0.0))
          (with-foreign-array (tmp 'sample)
            (dotimes (i size)
              (setf (smp-ref foreign-pointer i) +sample-zero+)
              (loop for (num amp phs dc) in pl do
                      (incf (smp-ref foreign-pointer i)
                            (partial-ref num amp phs dc i size tmp)))
              (setf abs-value (abs (smp-ref foreign-pointer i)))
              (when (> abs-value max) (setf max abs-value))))
          (unless periodic-p
            (setf (smp-ref foreign-pointer size) (smp-ref foreign-pointer 0)))
          (values foreign-pointer
                  (if (zerop max) max (/ max))
                  normalize-p))))))

;;; BUZZ and GBUZZ are inspired by GEN11 of Csound.
(defun buzz (num-harm)
  (declare (type (integer 1 100000) num-harm))
  (let ((two-nh-plus-one (1+ (ash num-harm 1))))
    (declare (type positive-fixnum two-nh-plus-one))
    (lambda (foreign-pointer size)
      (declare (type foreign-pointer foreign-pointer)
               (type non-negative-fixnum size))
      (with-samples ((mult (/ 0.5 num-harm))
                     (pi-step (/ pi size))
                     angle num denom)
        (dotimes (i size foreign-pointer)
          (setf angle (* i pi-step))
          (setf denom (sin (the limited-sample angle)))
          (setf (smp-ref foreign-pointer i)
                (cond ((zerop denom) (sample 1))
                      (t (setf num (sin (the limited-sample
                                          (* two-nh-plus-one angle))))
                         (* mult (- (/ num denom) 1.0))))))))))

(defun gbuzz (num-harm &optional (lowest-harm 1) (mul 1))
  "Return a function called to fill a foreign array with a composite
waveform made up of harmonically related sine partials.

NUM-HARM is the number of harmonics.

LOWEST-HARM is the lowest harmonic and defaults to 1.

MUL is a real number and defaults to 1. It specifies the multiplier
of a power series where

    (* strength-coeff (expt mul n))

is the strength of the partial (+ lowest-harm n).

The returned function takes two arguments, the foreign pointer to
the sample data and the data size, and returns the foreign array."
  (declare (type (integer 1 100000) num-harm) (type fixnum lowest-harm))
  (if (and (= lowest-harm 1) (= mul 1))
      (buzz num-harm)
      (let* ((c1 (1- lowest-harm))
             (c2 (+ lowest-harm num-harm))
             (c3 (1- c2))
             (mul (sample mul)))
        (declare (type non-negative-fixnum c1 c2 c3) (type sample mul))
        (lambda (foreign-pointer size)
          (declare (type foreign-pointer foreign-pointer)
                   (type non-negative-fixnum size))
          (incudine-optimize
            (with-samples* ((abs-mul (abs mul))
                            (two-mul (+ mul mul))
                            (squared-mul-plus-one (+ (* mul mul) (sample 1)))
                            (c2-mult (expt (the non-negative-sample abs-mul)
                                           num-harm))
                            c3-mult scale twopi-step angle num denom)
              (when (and (minusp mul) (plusp (logand num-harm 1)))
                (setf c2-mult (- c2-mult)))
              (setf c3-mult (* c2-mult mul))
              (setf scale (let ((one (sample 1)))
                            (if (and (> abs-mul (sample 0.999d0))
                                     (< abs-mul (sample 1.001d0)))
                                (/ one num-harm)
                                (/ (- one abs-mul) (- one (abs c2-mult))))))
              (setf twopi-step (/ +twopi+ size))
              (dotimes (i size foreign-pointer)
                (setf angle (* i twopi-step)
                      denom (- squared-mul-plus-one
                               (* two-mul (cos (the limited-sample angle)))))
                (setf (smp-ref foreign-pointer i)
                      (cond ((or (> denom (sample 1d-5))
                                 (< denom (sample -1d-5)))
                             (setf num (+ (- (cos (the limited-sample
                                                       (* lowest-harm angle)))
                                             (* mul (cos (the limited-sample
                                                           (* c1 angle))))
                                             (* c2-mult (cos (the limited-sample
                                                               (* c2 angle)))))
                                          (* c3-mult (cos (the limited-sample
                                                            (* c3 angle))))))
                             (* scale (/ num denom)))
                            (t (sample 1)))))))))))

(defun chebyshev-1 (strength-list &key (xmin -1) (xmax 1) (offset-p t)
                    (normalize-p t))
  "Return a function called to fill a foreign array by using a
weighted combination of Chebyshev polynomials of the first kind.

The Chebyshev functions take values between XMIN and XMAX (-1 and 1 by
default).

If OFFSET-P is T (default), the center value of the wavetable is zero.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns three values: the foreign
array, the scale factor to normalize the samples and the boolean
NORMALIZE-P to specify whether the normalization is necessary.

Example:

    \(in-package :scratch)

    (define-vug shaper (in (buf buffer))
      (with ((n (ash (buffer-frames buf) -1)))
        (declare (fixnum n))
        (buffer-read buf (+ (* in n) n) :interpolation :linear)))

    (defvar *cheb*
      (make-buffer
        512 :fill-function (gen:chebyshev-1 '(1 1/2 1/3 1/4 1/5))))

    (dsp! cheby-test (freq amp dur)
      (:defaults 440 .3 4)
      (out (* amp (shaper (sine freq (line 0 .99 dur #'free) 0) *cheb*))))"
  (declare (type list strength-list) (type real xmin xmax)
           (type boolean offset-p normalize-p))
  (lambda (foreign-array size)
    (declare (type foreign-pointer foreign-array)
             (type non-negative-fixnum size))
    (incudine-optimize
      (declare #.*reduce-warnings*)
      (with-samples ((phase-init (sample xmin))
                     (phase 0.0)
                     (phase-inc 0.0)
                     (offset 0.0)
                     (abs-value 0.0)
                     (max-value 0.0))
        ;; Clear the buffer.
        (incudine.external:foreign-zero-sample foreign-array size)
        (setf phase-inc (sample (/ (- xmax xmin) size)))
        (do ((scale-list strength-list (cdr scale-list))
             (partial 1 (1+ partial)))
            ((null scale-list))
          (declare (type list scale-list) (type positive-fixnum partial))
          (let ((scale (car scale-list)))
            (declare (type real scale))
            (setf phase phase-init)
            (unless (zerop scale)
              (when offset-p
                (setf offset (* scale (cos (the limited-sample
                                             (* partial +half-pi+))))))
              (dotimes (i size)
                (incf (smp-ref foreign-array i)
                      (- (* scale (cheb partial phase)) offset))
                (incf phase phase-inc)))))
        (setf max-value +sample-zero+)
        (dotimes (i size)
          (setf abs-value (abs (smp-ref foreign-array i)))
          (when (> abs-value max-value)
            (setf max-value abs-value)))
        (values foreign-array
                (if (zerop max-value) max-value (/ max-value))
                normalize-p)))))
