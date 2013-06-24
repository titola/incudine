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

;;;
;;; Example: x^2 - 3      with x in [-1, 1]
;;;
;;;   (polynomial -1 1 2 0 -3)
;;;
(defun polynomial (x1 x2 &rest coefficients)
  (let* ((x1 (coerce x1 'sample))
         (range (abs (- x2 x1)))
         (coeffs (or (mapcar (lambda (x) (coerce x 'sample))
                             coefficients)
                     `(,+sample-zero+))))
    (declare (type sample x1 range) (type cons coeffs))
    (lambda (c-array size)
      (declare (type foreign-pointer c-array)
               (type non-negative-fixnum size)
               #.*standard-optimize-settings*)
      (with-samples* ((scale (/ range size))
                      (xloc (sample->fixnum (/ x1 scale)))
                      x sum)
        (dotimes (i size c-array)
          (setf x (* xloc scale))
          (incf xloc)
          (setf sum (car coeffs))
          (setf (mem-aref c-array 'sample i)
                (dolist (c (cdr coeffs) sum)
                  (setf sum (+ (* sum x) (the sample c))))))))))
