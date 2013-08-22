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

(in-package :incudine)

(defvar *number-of-input-bus-channels* 2)
(declaim (type channel-number *number-of-input-bus-channels*))

(defvar *number-of-output-bus-channels* 2)
(declaim (type channel-number *number-of-output-bus-channels*))

(defvar *number-of-bus-channels* 4096)
(declaim (type bus-number *number-of-bus-channels*))

(defvar %number-of-input-bus-channels
  ;; It is possible to use (AUDIO-IN CURRENT-CHANNEL)
  ;; without the risk to read a different bus
  (max *number-of-input-bus-channels*
       *number-of-output-bus-channels*))

(defvar *bus-channels-size* (+ *number-of-output-bus-channels*
                               %number-of-input-bus-channels
                               *number-of-bus-channels*))
(declaim (type non-negative-fixnum *bus-channels-size*))

(defvar *bus-channels* (foreign-alloc 'sample
                         :count *bus-channels-size*
                         :initial-element +sample-zero+))
(declaim (type foreign-pointer *bus-channels*))

(defvar *input-pointer*
  (inc-pointer *bus-channels*
               (* *number-of-output-bus-channels*
                  +foreign-sample-size+)))
(declaim (type foreign-pointer *input-pointer*))

(defvar *output-pointer* *bus-channels*)
(declaim (type foreign-pointer *output-pointer*))

(defvar *bus-pointer*
  (inc-pointer *bus-channels*
               (* (+ %number-of-input-bus-channels
                     *number-of-output-bus-channels*)
                  +foreign-sample-size+)))
(declaim (type foreign-pointer *bus-pointer*))

(defvar *output-peak-values* (foreign-alloc 'sample
                               :count *number-of-output-bus-channels*
                               :initial-element +sample-zero+))
(declaim (type foreign-pointer *output-peak-values*))

(defvar *out-of-range-counter* (make-array *number-of-output-bus-channels*
                                           :initial-element 0))
(declaim (type simple-vector *out-of-range-counter*))

(declaim (inline bus))
(defun bus (num)
  (declare (type bus-number num))
  (data-ref *bus-pointer* num))

(declaim (inline set-bus))
(defun set-bus (num value)
  (declare (type bus-number num))
  (setf (data-ref *bus-pointer* num) (sample value)))

(defsetf bus set-bus)

(declaim (inline audio-in))
(defun audio-in (channel)
  (declare (type channel-number channel))
  (data-ref *input-pointer* channel))

(declaim (inline audio-out))
(defun audio-out (channel)
  (declare (type channel-number channel))
  (data-ref *output-pointer* channel))

(declaim (inline set-audio-out))
(defun set-audio-out (channel value)
  (declare (type channel-number channel))
  (setf (data-ref *output-pointer* channel) (sample value)))

(defsetf audio-out set-audio-out)

(defun update-peak-values (chan)
  (declare #.*standard-optimize-settings*
           (type channel-number chan))
  (let ((value (mem-aref incudine::*output-pointer* 'sample chan)))
    (declare (type sample value))
    (when (> value (mem-aref *output-peak-values* 'sample chan))
      (setf (mem-aref *output-peak-values* 'sample chan) value))
    (when (> value (sample 1))
      (setf #1=(svref *out-of-range-counter* chan)
            (the positive-fixnum
              (1+ (the positive-fixnum #1#)))))
    (values)))

(declaim (inline peak-info))
(defun peak-info (chan)
  (declare (type channel-number chan))
  (values (data-ref *output-peak-values* chan)
          (svref *out-of-range-counter* chan)))

(defun print-peak-info (&optional (channels *number-of-output-bus-channels*)
                        (stream *standard-output*))
  (format stream "~11tpeak amps:  ")
  (foreach-channel (ch channels)
    (format stream "~8,3,F  " (data-ref *output-peak-values* ch)))
  (format stream "~%samples out of range:  ")
  (foreach-channel (ch channels)
    (format stream "~8,,D  " (svref *out-of-range-counter* ch)))
  (terpri))

(declaim (inline %reset-peak-values))
(defun %reset-peak-meters ()
  (foreach-channel (chan *number-of-output-bus-channels*)
    (setf (data-ref *output-peak-values* chan) +sample-zero+)
    (setf (svref *out-of-range-counter* chan) 0)))

(defun reset-peak-meters ()
  (at 0 #'%reset-peak-meters))

(defun set-number-of-channels (inputs outputs)
  "Safe way to change the number of the input/output channels."
  (declare (type channel-number inputs outputs))
  (unless (and (= inputs *number-of-input-bus-channels*)
               (= outputs *number-of-output-bus-channels*))
    (let* ((rt-started-p (eq (rt-status) :started))
           (old-outputs *number-of-output-bus-channels*)
           (safe-inputs (max inputs outputs))
           (channels (+ safe-inputs outputs *number-of-bus-channels*)))
      (rt-stop)
      (setf *number-of-input-bus-channels* inputs
            *number-of-output-bus-channels* outputs
            %number-of-input-bus-channels safe-inputs)
      (unless (= channels *bus-channels-size*)
        (msg info "Realloc the foreign array for the bus channels")
        (setf *bus-channels-size* channels)
        (foreign-free *bus-channels*)
        (setf *bus-channels* (foreign-alloc 'sample
                                            :count *bus-channels-size*
                                            :initial-element +sample-zero+))
        (setf *output-pointer* *bus-channels*))
      (setf *bus-pointer*
            (inc-pointer *bus-channels* (* (+ safe-inputs outputs)
                                           +foreign-sample-size+)))
      (unless (= outputs old-outputs)
        (setf *input-pointer*
              (inc-pointer *bus-channels* (* outputs +foreign-sample-size+)))
        (msg info "Realloc the foreign array for the output peak values")
        (foreign-free *output-peak-values*)
        (setf *output-peak-values* (foreign-alloc 'sample :count outputs
                                                  :initial-element +sample-zero+))
        (setf *out-of-range-counter* (make-array outputs :initial-element 0)))
      (when rt-started-p
        (msg info "Restart realtime")
        (rt-start)))))
