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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:envelope-data
     incudine::envelope-data-size
     incudine:envelope-duration
     incudine::%segment-init
     incudine::%segment-update-level)))

(defun envelope (env &optional (periodic-p t) normalize-p)
  (declare (type incudine:envelope env)
           (type boolean periodic-p normalize-p))
  (lambda (c-array size)
    (declare (type foreign-pointer c-array)
             (type non-negative-fixnum size)
             #.*standard-optimize-settings*
             #.*reduce-warnings*)
    (let* ((size (if periodic-p size (1- size)))
           ;; Used at the end of the envelope to fix roundoff errors
           (size-remained size)
           (env-data (envelope-data env))
           (data-size (envelope-data-size env))
           (guard-point (- data-size 3))
           (index 0)
           (dur 0)
           (remain 0))
      (declare (type non-negative-fixnum size size-remained data-size
                     guard-point index dur remain))
      (with-samples* ((level (data-ref env-data 0))
                      (time-scale (/ size (envelope-duration env)))
                      (end level)
                      (max level)
                      (grow 0.0)
                      (a2 0.0)
                      (b1 0.0)
                      (y1 0.0)
                      (y2 0.0)
                      (curve incudine:+seg-lin-func+))
        (dotimes (i size)
          (setf (mem-aref c-array 'sample i)
                (if (zerop remain)
                    (cond ((>= (incf index) data-size)
                           (nrt-msg error "the envelope ~A is corrupted" env)
                           level)
                          (t (if (= index guard-point)
                                 (setf dur (max 1 size-remained))
                                 (setf dur (max 1 (sample->fixnum
                                                   (* (data-ref env-data index)
                                                      time-scale)))
                                       size-remained (- size-remained dur)))
                             (setf remain (1- dur)
                                   index (1+ index)
                                   level end
                                   end (data-ref env-data index)
                                   index (1+ index)
                                   curve (data-ref env-data index))
                             (when (> end max) (setf max end))
                             (%segment-init level end dur curve grow a2 b1 y1 y2)
                             level))
                    (progn (decf remain)
                           (%segment-update-level level curve grow a2 b1 y1 y2)
                           level))))
        (unless periodic-p
          (setf (mem-aref c-array 'sample size) end))
        (values c-array
                ;; Factor to scale the amplitude
                (/ max)
                normalize-p)))))
