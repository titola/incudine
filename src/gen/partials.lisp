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

(in-package :incudine.gen)

(declaim (inline partial-ref))
(defun partial-ref (partial-number amp phase dc index size tmp-var)
  (declare (type non-negative-fixnum partial-number index size)
           (type foreign-pointer tmp-var ))
  (setf (smp-ref tmp-var 0)
        (+ dc (* amp (sin (the limited-sample
                            (+ (/ (* +twopi+ index partial-number) size)
                               (* +twopi+ phase))))))))

(defun complete-partial-list (lst)
  (declare (type list lst))
  (do* ((i 1 (1+ i))
        (par-list lst (cdr par-list))
        (par #1=(car par-list) #1#)
        (acc nil))
       ((null par) (nreverse acc))
    (declare (type list par-list acc)
             (type positive-fixnum i))
    (cond ((and (numberp par) (>= par 0))
           (push `(,i ,par ,+sample-zero+ ,+sample-zero+) acc))
          ((and (consp par) (every #'numberp par))
           (push `(,(car par)
                    ,(or (second par) (sample 1))
                    ,(or (third par) +sample-zero+)
                    ,(or (fourth par) +sample-zero+))
                 acc)
           (setf i (car par)))
          (t (decf i)))))

;;; Inspired by GEN09, GEN10 and GEN19 of Csound
(defun partials (lst &optional (periodic-p t) (normalize-p t))
  (declare #.*standard-optimize-settings*
           (type list lst) (type boolean periodic-p normalize-p)
           #.*reduce-warnings*)
  (let ((pl (complete-partial-list lst)))
    (declare (type list pl))
    (lambda (c-array size)
      (declare (type foreign-pointer c-array)
               (type non-negative-fixnum size))
      (let ((size (if periodic-p size (1- size))))
        (declare (type non-negative-fixnum size))
        (with-foreign-object (tmp 'sample)
          (with-samples (value abs-value (max 0.0))
            (dotimes (i size)
              (setf (smp-ref c-array i)
                    (reduce #'+
                            (mapcar (lambda (x)
                                      (destructuring-bind (num amp phs dc) x
                                        (partial-ref num amp phs dc
                                                     i size tmp)))
                                    pl)))
              (setf abs-value (abs (smp-ref c-array i)))
              (when (> abs-value max)
                (setf max abs-value)))
            (unless periodic-p
              (setf (smp-ref c-array size)
                    (smp-ref c-array 0)))
            (values c-array
                    ;; Factor to scale the amplitude
                    (/ max)
                    normalize-p)))))))

;;; BUZZ and GBUZZ are inspired by GEN11 of Csound
(defun buzz (num-harm)
  (declare (type (integer 1 100000) num-harm))
  (let ((two-nh-plus-one (1+ (ash num-harm 1))))
    (declare (type positive-fixnum two-nh-plus-one))
    (lambda (c-array size)
      (declare (type foreign-pointer c-array)
               (type non-negative-fixnum size))
      (with-samples ((mult (/ 0.5 num-harm))
                     (pi-step (/ pi size))
                     angle num denom)
        (dotimes (i size c-array)
          (setf angle (* i pi-step))
          (setf denom (sin (the limited-sample angle)))
          (setf (smp-ref c-array i)
                (cond ((zerop denom) (sample 1))
                      (t (setf num (sin (the limited-sample
                                          (* two-nh-plus-one angle))))
                         (* mult (- (/ num denom) 1.0))))))))))

(defun gbuzz (num-harm &optional (lowest-harm 1) (mul 1))
  (declare (type (integer 1 100000) num-harm) (type fixnum lowest-harm))
  (if (and (= lowest-harm 1) (= mul 1))
      (buzz num-harm)
      (let* ((c1 (1- lowest-harm))
             (c2 (+ lowest-harm num-harm))
             (c3 (1- c2))
             (mul (sample mul)))
        (declare (type non-negative-fixnum c1 c2 c3)
                 (type sample mul))
        (lambda (c-array size)
          (declare (type foreign-pointer c-array)
                   (type non-negative-fixnum size)
                   #.*standard-optimize-settings*)
          (with-samples* ((abs-mul (abs mul))
                          (two-mul (+ mul mul))
                          (squared-mul-plus-one (+ (* mul mul) (sample 1)))
                          (c2-mult (expt (the (sample #.+sample-zero+) abs-mul) num-harm))
                          c3-mult scale twopi-step angle num denom)
            (when (and (minusp mul)
                       (plusp (logand num-harm 1)))
              (setf c2-mult (- c2-mult)))
            (setf c3-mult (* c2-mult mul)
                  scale (let ((one (sample 1)))
                          (if (and (> abs-mul (sample 0.999))
                                   (< abs-mul (sample 1.001)))
                              (/ one num-harm)
                              (/ (- one abs-mul)
                                 (- one (abs c2-mult)))))
                  twopi-step (/ +twopi+ size))
            (dotimes (i size c-array)
              (setf angle (* i twopi-step)
                    denom (- squared-mul-plus-one (* two-mul
                                                     (cos (the limited-sample angle)))))
              (setf (smp-ref c-array i)
                    (cond ((or (> denom (sample 1.e-5))
                               (< denom (sample -1.e-5)))
                           (setf num (+ (- (cos (the limited-sample (* lowest-harm angle)))
                                           (* mul (cos (the limited-sample (* c1 angle))))
                                           (* c2-mult (cos (the limited-sample (* c2 angle)))))
                                        (* c3-mult (cos (the limited-sample (* c3 angle))))))
                           (* scale (/ num denom)))
                          (t (sample 1))))))))))

;;; Chebyshev polynomials of the first kind
(defun chebyshev-1 (strength-list &key (xmin -1) (xmax 1) (offset-p t)
                    (normalize-p t))
  (declare (type list strength-list) (type real xmin xmax)
           (type boolean offset-p normalize-p)
           #.*standard-optimize-settings*
           #.*reduce-warnings*)
  (with-samples ((phase-init (sample xmin))
                 (phase 0.0)
                 (phase-inc 0.0)
                 (offset 0.0)
                 (abs-value 0.0)
                 (max-value 0.0))
    (lambda (c-array size)
      (declare (type foreign-pointer c-array)
               (type non-negative-fixnum size))
      ;; Clear the buffer
      (incudine.external:foreign-zero-sample c-array size)
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
              (incf (smp-ref c-array i)
                    (+ (* scale (cos (the limited-sample
                                       (* partial (the limited-sample
                                                    (acos phase))))))
                       offset))
              (incf phase phase-inc)))))
      (setf max-value +sample-zero+)
      (dotimes (i size)
        (setf abs-value (abs (smp-ref c-array i)))
        (when (> abs-value max-value)
          (setf max-value abs-value)))
      (values c-array
              (/ max-value)
              normalize-p))))
