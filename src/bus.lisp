;;; Copyright (c) 2013-2014 Tito Latini
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

(defvar *input-increment-bytes*
  (* *number-of-input-bus-channels* +foreign-sample-size+))
(declaim (type non-negative-fixnum *input-increment-bytes*))

(defvar *output-increment-bytes*
  (* *number-of-output-bus-channels* +foreign-sample-size+))
(declaim (type non-negative-fixnum *output-increment-bytes*))

(defvar *number-of-bus-channels* 4096)
(declaim (type bus-number *number-of-bus-channels*))

(defvar %number-of-input-bus-channels
  ;; It is possible to use (AUDIO-IN CURRENT-CHANNEL) without risks.
  (max *number-of-input-bus-channels* *number-of-output-bus-channels*))

(defvar *%input-pointer*
  (foreign-alloc-sample (* *max-buffer-size*
                           %number-of-input-bus-channels)))
(declaim (type foreign-pointer *%input-pointer*))

(defvar *input-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%input-pointer*))
(declaim (type foreign-pointer *input-pointer*))

(defmacro input-pointer ()
  `(mem-ref *input-pointer* :pointer))

(defvar *%output-pointer*
  (foreign-alloc-sample (* *max-buffer-size*
                           *number-of-output-bus-channels*)))
(declaim (type foreign-pointer *%output-pointer*))

(defvar *output-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%output-pointer*))
(declaim (type foreign-pointer *output-pointer*))

(defmacro output-pointer ()
  `(mem-ref *output-pointer* :pointer))

(defvar *bus-pointer* (foreign-alloc-sample *number-of-bus-channels*))
(declaim (type foreign-pointer *bus-pointer*))

(defmacro reset-io-pointers ()
  `(setf (input-pointer) *%input-pointer*
         (output-pointer) *%output-pointer*))

(defmacro inc-io-pointers (delta)
  (with-gensyms (offset)
    `(let ((,offset ,delta))
       (incf-pointer (input-pointer)
                     (the fixnum (* ,offset *input-increment-bytes*)))
       (incf-pointer (output-pointer)
                     (the fixnum (* ,offset *output-increment-bytes*))))))

(defvar *output-peak-values*
  (foreign-alloc-sample *number-of-output-bus-channels*))
(declaim (type foreign-pointer *output-peak-values*))

(defvar *out-of-range-counter*
  (make-array *number-of-output-bus-channels* :initial-element 0))
(declaim (type simple-vector *out-of-range-counter*))

(declaim (inline bus))
(defun bus (num)
  (declare (type bus-number num))
  (smp-ref *bus-pointer* num))

(declaim (inline set-bus))
(defun set-bus (num value)
  (declare (type bus-number num))
  (setf (smp-ref *bus-pointer* num) (sample value)))

(defsetf bus set-bus)

(declaim (inline audio-in))
(defun audio-in (channel &optional (frame 0))
  (declare (type channel-number channel)
           (type non-negative-fixnum frame))
  (smp-ref (input-pointer) (the non-negative-fixnum (+ frame channel))))

(declaim (inline audio-out))
(defun audio-out (channel &optional (frame 0))
  (declare (type channel-number channel)
           (type non-negative-fixnum frame))
  (smp-ref (output-pointer) (the non-negative-fixnum (+ frame channel))))

(declaim (inline set-audio-out))
(defun set-audio-out (channel frame value)
  (declare (type channel-number channel)
           (type non-negative-fixnum frame)
           (type real value))
  (setf (smp-ref (output-pointer) (the non-negative-fixnum
                                       (+ frame channel)))
        (sample value)))

(defsetf audio-out (channel &optional (frame 0)) (value)
  `(set-audio-out ,channel ,frame ,value))

(defun update-peak-values (chan)
  (declare #.*standard-optimize-settings*
           (type channel-number chan))
  (let ((value (mem-aref (output-pointer) 'sample chan)))
    (declare (type sample value))
    (when (> value (smp-ref *output-peak-values* chan))
      (setf (smp-ref *output-peak-values* chan) value))
    (when (> value (sample 1))
      (setf #1=(svref *out-of-range-counter* chan)
            (the positive-fixnum (1+ (the positive-fixnum #1#)))))
    (values)))

(declaim (inline peak-info))
(defun peak-info (chan)
  (declare (type channel-number chan))
  (values (smp-ref *output-peak-values* chan)
          (svref *out-of-range-counter* chan)))

(defun print-peak-info (&optional (channels *number-of-output-bus-channels*)
                        (stream *logger-stream*))
  (fresh-line stream)
  (format stream "~11Tpeak amps:  ")
  (dochannels (ch channels)
    (format stream "~8,3,F  " (smp-ref *output-peak-values* ch)))
  (format stream "~%samples out of range:  ")
  (dochannels (ch channels)
    (format stream "~8,,D  " (svref *out-of-range-counter* ch)))
  (terpri stream))

(declaim (inline %reset-peak-values))
(defun %reset-peak-meters ()
  (dochannels (chan *number-of-output-bus-channels*)
    (setf (smp-ref *output-peak-values* chan) +sample-zero+)
    (setf (svref *out-of-range-counter* chan) 0)))

(defun reset-peak-meters ()
  (at 0 #'%reset-peak-meters))

(defun set-number-of-channels (inputs outputs)
  "Safe way to change the number of the input/output channels."
  (declare (type channel-number inputs outputs))
  (unless (and (= inputs *number-of-input-bus-channels*)
               (= outputs *number-of-output-bus-channels*))
    (let ((rt-started-p (eq (rt-status) :started)))
      (rt-stop)
      (let ((old-outputs *number-of-output-bus-channels*))
        (setf *number-of-input-bus-channels* inputs
              *number-of-output-bus-channels* outputs
              %number-of-input-bus-channels (max inputs outputs))
        (foreign-free *%input-pointer*)
        (setf *%input-pointer* (foreign-alloc-sample
                                 (* *max-buffer-size*
                                    %number-of-input-bus-channels)))
        (setf (input-pointer) *%input-pointer*)
        (setf *input-increment-bytes*
              (* *number-of-input-bus-channels* +foreign-sample-size+))
        (unless (= outputs old-outputs)
          (foreign-free *%output-pointer*)
          (setf *%output-pointer* (foreign-alloc-sample
                                    (* *max-buffer-size*
                                       *number-of-output-bus-channels*)))
          (setf (output-pointer) *%output-pointer*)
          (setf *output-increment-bytes*
                (* *number-of-output-bus-channels* +foreign-sample-size+))
          (msg info "Realloc the foreign array for the output peak values")
          (foreign-free *output-peak-values*)
          (setf *output-peak-values* (foreign-alloc-sample outputs))
          (setf *out-of-range-counter* (make-array outputs :initial-element 0))))
      (and rt-started-p (rt-status)))))
