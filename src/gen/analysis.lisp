;;; Copyright (c) 2015 Tito Latini
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
  (declare (type (or incudine.analysis:analysis incudine.analysis:abuffer) obj))
  (lambda (c-array size)
    (unless (incudine:free-p obj)
      (multiple-value-bind (src-ptr src-size)
          (if (incudine.analysis:abuffer-p obj)
              (values (incudine.analysis:abuffer-data obj)
                      (incudine.analysis:abuffer-size obj))
              (values (incudine.analysis:analysis-data obj)
                      (incudine.analysis::analysis-output-size obj)))
        (incudine.external:foreign-copy-samples c-array src-ptr
                                                (min size src-size))))
    c-array))
