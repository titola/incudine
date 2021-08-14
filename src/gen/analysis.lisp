;;; Copyright (c) 2015-2021 Tito Latini
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

(defun analysis (obj)
  "Return a function called to fill a foreign array with the analysis
data of an ANALYSIS or ABUFFER structure passed as argument.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (declare (type (or incudine.analysis:analysis incudine.analysis:abuffer) obj))
  (lambda (foreign-array size)
    (declare (type foreign-pointer foreign-array)
             (type non-negative-fixnum size))
    (incudine-optimize
      (declare #.*reduce-warnings*)
      (unless (incudine:free-p obj)
        (multiple-value-bind (src-ptr src-size)
            (if (incudine.analysis:abuffer-p obj)
                (values (incudine.analysis:abuffer-data obj)
                        (incudine.analysis:abuffer-size obj))
                (values (incudine.analysis:analysis-output-buffer obj)
                        (incudine.analysis:analysis-output-buffer-size obj)))
          (cond ((incudine.analysis::real-output-p obj)
                 ;; Possible normalization from FILL-BUFFER.
                 (with-samples (abs-value max-value)
                   (dotimes (i (min size src-size))
                     (setf abs-value
                           (abs (setf (smp-ref foreign-array i)
                                      (smp-ref src-ptr i))))
                     (when (< max-value abs-value)
                       (setf max-value abs-value)))
                   (values foreign-array
                           (if (zerop max-value) max-value (/ max-value)))))
                (t
                 (incudine.external:foreign-copy-samples
                   foreign-array src-ptr (min size src-size))
                 foreign-array)))))))
