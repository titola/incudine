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

(defmacro defwindow (name (c-array-var size-var) &body body)
  `(defun ,name ()
     (lambda (,c-array-var ,size-var)
       (declare #.*standard-optimize-settings*
                (type foreign-pointer ,c-array-var)
                (type non-negative-fixnum ,size-var))
       ,@body)))

(defwindow hanning (c-array size)
  (with-samples ((delta (/ +twopi+ size))
                 (phase +sample-zero+))
    (dotimes (i size c-array)
      (setf (smp-ref c-array i)
            (* 0.5 (- 1.0 (cos (the limited-sample
                                 phase)))))
      (incf phase delta))))

(defwindow hamming (c-array size)
  (with-samples ((delta (/ +twopi+ size))
                 (phase +sample-zero+))
    (dotimes (i size c-array)
      (setf (smp-ref c-array i)
            (- 0.54 (* 0.46 (cos (the limited-sample
                                   phase)))))
      (incf phase delta))))

(defwindow blackman (c-array size)
  (with-samples ((delta (/ +twopi+ size))
                 (phase +sample-zero+))
    (dotimes (i size c-array)
      (setf (smp-ref c-array i)
            (+ (- 0.42 (* 0.5 (cos (the limited-sample
                                     phase))))
               (* 0.08 (cos (the limited-sample
                              (* 2 phase))))))
      (incf phase delta))))

(defwindow sine-window (c-array size)
  (with-samples ((winc (/ (sample pi) size)))
    (dotimes (i size c-array)
      (setf (smp-ref c-array i)
            (sin (the limited-sample
                   (* i winc)))))))

(defwindow bartlett (c-array size)
  (let ((half-size (/ size 2)))
    (declare (type non-negative-fixnum half-size))
    (with-samples ((half-recip (/ (sample half-size))))
      (dotimes (i size c-array)
        (setf (smp-ref c-array i)
              (if (< i half-size)
                  (* i half-recip)
                  (* (- size i) half-recip)))))))
