;;; Copyright (c) 2013-2016 Tito Latini
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

(in-package :incudine.external)

(cffi:defcfun ("pa_initialize" rt-audio-init) :int
  (input-channels :unsigned-int)
  (output-channels :unsigned-int)
  (frames-per-buffer :unsigned-long)
  (client-name :pointer))

(cffi:defcfun ("pa_start" rt-audio-start) :int)

(cffi:defcfun ("pa_stop" rt-audio-stop) :int)

(declaim (inline rt-set-io-buffers))
(cffi:defcfun ("pa_set_lisp_io" rt-set-io-buffers) :void
  (input :pointer)
  (output :pointer))

(declaim (inline rt-cycle-begin))
(cffi:defcfun ("pa_cycle_begin" rt-cycle-begin) :unsigned-long)

(declaim (inline rt-cycle-end))
(cffi:defcfun ("pa_cycle_end" rt-cycle-end) :void
  (nframes :unsigned-long))

(declaim (inline rt-condition-wait))
(cffi:defcfun ("pa_condition_wait" rt-condition-wait) :void)

(declaim (inline rt-transfer-to-c-thread))
(cffi:defcfun ("pa_transfer_to_c_thread" rt-transfer-to-c-thread) :void)

(declaim (inline rt-set-busy-state))
(cffi:defcfun ("pa_set_lisp_busy_state" rt-set-busy-state) :void
  (status :boolean))

(cffi:defcfun ("pa_get_error_msg" rt-get-error-msg) :string)

(declaim (inline rt-buffer-size))
(cffi:defcfun ("pa_get_buffer_size" rt-buffer-size) :int)

(cffi:defcfun ("pa_get_sample_rate" rt-sample-rate) sample)

(cffi:defcstruct pa-device-info
  (struct-version :int)
  (name :string)
  (host-api :int)
  (max-input-channels :int)
  (max-output-channels :int)
  (default-low-input-latency :double)
  (default-low-output-latency :double)
  (default-high-input-latency :double)
  (default-high-output-latency :double)
  (default-sample-rate :double))

(defun pa-device-info-name (id)
  (cffi:foreign-slot-value
   (cffi:foreign-funcall "Pa_GetDeviceInfo" :int id :pointer)
   '(:struct pa-device-info) 'name))

(defun portaudio-device-info (&optional (stream *standard-output*))
  (when (zerop (cffi:foreign-funcall "Pa_Initialize" :int))
    (unwind-protect
         (let ((count (cffi:foreign-funcall "Pa_GetDeviceCount" :int)))
           (when (plusp count)
             (dotimes (i count)
               (format stream "~D~3T~S~%" i (pa-device-info-name i)))))
      (cffi:foreign-funcall "Pa_Terminate" :int))))

(cffi:defcfun "pa_set_devices" :void
  (input :int)
  (output :int))

(defvar incudine.config::*portaudio-input-device* -1)
(defvar incudine.config::*portaudio-output-device* -1)

(defun portaudio-set-device (output &optional input)
  (declare (type fixnum output) (type (or fixnum null) input))
  (setf incudine.config::*portaudio-input-device* (or input output)
        incudine.config::*portaudio-output-device* output))
