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

(in-package :incudine)

;; Samples reserved after the allocated audio busses to avoid a segfault
;; if the user accidentally requires a value after the last frame.
(define-constant +io-bus-channels-pad+ 1024)

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

(defun alloc-bus-pointer (type)
  (declare (type (member bus input output) type))
  (foreign-alloc-sample
   (+ (if (member type '(input output))
          (* *max-buffer-size*
             (max *number-of-input-bus-channels*
                  *number-of-output-bus-channels*))
          *number-of-bus-channels*)
      +io-bus-channels-pad+)))

(defvar *%input-pointer* (alloc-bus-pointer 'input))
(declaim (type foreign-pointer *%input-pointer*))

(defvar *input-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%input-pointer*))
(declaim (type foreign-pointer *input-pointer*))

(defmacro input-pointer ()
  `(mem-ref *input-pointer* :pointer))

(defvar *%output-pointer* (alloc-bus-pointer 'output))
(declaim (type foreign-pointer *%output-pointer*))

(defvar *output-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%output-pointer*))
(declaim (type foreign-pointer *output-pointer*))

(defmacro output-pointer ()
  `(mem-ref *output-pointer* :pointer))

(defvar *bus-pointer* (alloc-bus-pointer 'bus))
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
  "Return the value of the bus number NUM. Setfable."
  (declare (type bus-number num))
  (smp-ref *bus-pointer* num))

(declaim (inline set-bus))
(defun set-bus (num value)
  (declare (type bus-number num))
  (setf (smp-ref *bus-pointer* num) (sample value)))

(defsetf bus set-bus)

(declaim (inline audio-in))
(defun audio-in (channel &optional (frame 0))
  "Return the value of the CHANNEL of the input buffer FRAME.

If AUDIO-IN is used within a VUG definition, FRAME is ignored."
  (declare (type channel-number channel)
           (type non-negative-fixnum frame))
  (smp-ref (input-pointer)
           (the non-negative-fixnum
             (+ (the non-negative-fixnum
                  (* frame *number-of-input-bus-channels*))
                channel))))

(declaim (inline audio-out))
(defun audio-out (channel &optional (frame 0))
  "Return the value of the CHANNEL of the output buffer FRAME. Setfable.

If AUDIO-OUT is used within a VUG definition, FRAME is ignored."
  (declare (type channel-number channel)
           (type non-negative-fixnum frame))
  (smp-ref (output-pointer)
           (the non-negative-fixnum
             (+ (the non-negative-fixnum
                  (* frame *number-of-output-bus-channels*))
                channel))))

(declaim (inline set-audio-out))
(defun set-audio-out (channel frame value)
  (declare (type channel-number channel)
           (type non-negative-fixnum frame)
           (type real value))
  (setf (smp-ref (output-pointer)
                 (the non-negative-fixnum
                   (+ (the non-negative-fixnum
                        (* frame *number-of-output-bus-channels*))
                      channel)))
        (sample value)))

(defsetf audio-out (channel &optional (frame 0)) (value)
  `(set-audio-out ,channel ,frame ,value))

(defun update-peak-values (chan)
  (declare #.*standard-optimize-settings*
           (type channel-number chan))
  (let ((value (abs (mem-aref (output-pointer) 'sample chan))))
    (declare (type sample value))
    (when (> value (smp-ref *output-peak-values* chan))
      (setf (smp-ref *output-peak-values* chan) value))
    (when (> value (sample 1))
      (setf #1=(svref *out-of-range-counter* chan)
            (the positive-fixnum (1+ (the positive-fixnum #1#)))))
    (values)))

(declaim (inline peak-info))
(defun peak-info (chan)
  "Return peak information for the channel CHAN."
  (declare (type channel-number chan))
  (values (smp-ref *output-peak-values* chan)
          (svref *out-of-range-counter* chan)))

(defun print-peak-info (&optional (channels *number-of-output-bus-channels*)
                        (stream *logger-stream*))
  "Print the peak information.

The number of the channels defaults to *NUMBER-OF-OUTPUT-BUS-CHANNELS*.

The output stream is *LOGGER-STREAM* by default."
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
  "Reset the peak meters."
  (at 0 #'%reset-peak-meters))

(defmacro realloc-audio-bus-pointer (type)
  (destructuring-bind (ptr ptr-mac inc-bytes channels)
      (mapcar (lambda (fmt) (format-symbol "INCUDINE" fmt type))
              '("*%~A-POINTER*" "~A-POINTER" "*~A-INCREMENT-BYTES*"
                "*NUMBER-OF-~A-BUS-CHANNELS*"))
    `(progn
       (foreign-free ,ptr)
       (setf ,ptr (alloc-bus-pointer ',type))
       (setf (,ptr-mac) ,ptr)
       (setf ,inc-bytes (* ,channels +foreign-sample-size+)))))

(defun set-number-of-channels (inputs outputs)
  "Safe way to change the number of the input/output channels."
  (declare (type channel-number inputs outputs))
  (unless (and (= inputs *number-of-input-bus-channels*)
               (= outputs *number-of-output-bus-channels*))
    (let ((rt-started-p (eq (rt-status) :started)))
      (rt-stop)
      (let ((old-outputs *number-of-output-bus-channels*))
        (setf *number-of-input-bus-channels* inputs
              *number-of-output-bus-channels* outputs)
        (realloc-audio-bus-pointer input)
        (unless (= outputs old-outputs)
          (realloc-audio-bus-pointer output)
          (msg info "Realloc the foreign array for the output peak values")
          (setf *output-peak-values*
                (foreign-realloc *output-peak-values* 'sample :count outputs))
          (setf *out-of-range-counter* (make-array outputs :initial-element 0))))
      (and rt-started-p (rt-status)))))
