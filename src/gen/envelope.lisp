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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:envelope-data
     incudine::envelope-data-size
     incudine:envelope-duration
     incudine::%segment-init
     incudine::%segment-update-level)))

(defun envelope (env &key (periodic-p t) normalize-p)
  "Return a function called to fill a foreign array with the envelope
defined in the passed ENVELOPE structure ENV.

The returned function always refers to the same ENVELOPE structure.
It takes two arguments, the foreign pointer to the sample data and the
data size, and returns three values: the foreign array, the scale
factor to normalize the samples and the boolean NORMALIZE-P to specify
whether the normalization is necessary.

If PERIODIC-P is T (default), the resultant envelope is a cycle of a
periodic waveform."
  (declare (type incudine:envelope env) (type boolean periodic-p normalize-p))
  (lambda (foreign-array size)
    (declare (type foreign-pointer foreign-array)
             (type non-negative-fixnum size))
    (when (incudine:free-p env)
      (error 'incudine:incudine-memory-fault-error
             :format-control "Reference to a freed ENVELOPE."))
    (let* ((size (if periodic-p size (1- size)))
           ;; Used at the end of the envelope to fix roundoff errors.
           (size-remained size)
           (env-data (envelope-data env))
           (data-size (envelope-data-size env))
           (guard-point (- data-size 3))
           (index 0)
           (dur 0)
           (remain 0))
      (declare (type non-negative-fixnum size size-remained data-size
                     guard-point index dur remain)
               #.*standard-optimize-settings* #.*reduce-warnings*)
      (with-samples* ((level (smp-ref env-data 0))
                      (time-scale (/ size (envelope-duration env)))
                      (end level)
                      (abs-level (abs level))
                      (max abs-level)
                      (grow 0.0)
                      (a2 0.0)
                      (b1 0.0)
                      (y1 0.0)
                      (y2 0.0)
                      (curve incudine::+seg-lin-func+))
        (dotimes (i size)
          (setf (smp-ref foreign-array i)
                (if (zerop remain)
                    (cond ((>= (incf index) data-size)
                           (nrt-msg error "the envelope ~A is corrupted" env)
                           level)
                          (t (if (= index guard-point)
                                 (setf dur (max 1 size-remained))
                                 (setf dur (max 1 (sample->fixnum
                                                   (* (smp-ref env-data index)
                                                      time-scale)))
                                       size-remained (- size-remained dur)))
                             (setf remain (1- dur)
                                   index (1+ index)
                                   level end
                                   abs-level (abs level)
                                   end (smp-ref env-data index)
                                   index (1+ index)
                                   curve (smp-ref env-data index))
                             (when (> abs-level max) (setf max abs-level))
                             (%segment-init level end dur curve grow a2 b1 y1 y2)
                             level))
                    (progn (decf remain)
                           (%segment-update-level level curve grow a2 b1 y1 y2)
                           level))))
        (unless periodic-p (setf (smp-ref foreign-array size) end))
        (values foreign-array
                ;; Factor to scale the amplitude
                (/ max)
                normalize-p)))))
