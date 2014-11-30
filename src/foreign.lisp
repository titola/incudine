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

(in-package :incudine.external)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library (incudine
                                :search-path #.(load-time-value
                                                (or *compile-file-pathname*
                                                    *load-pathname*)))
    (:darwin "libincudine.dylib")
    (:unix "libincudine.so")
    (:cygwin "cygincudine-0.dll")
    (t (:default "libincudine")))

  (cffi:define-foreign-library fftw
    (:darwin #+double-samples "libfftw3.dylib" #-double-samples "libfftw3f.dylib")
    (:unix #+double-samples "libfftw3.so" #-double-samples "libfftw3f.so")
    (:cygwin #+double-samples "cygfftw3-0.dll" #-double-samples "cygfftw3f-0.dll")
    (t (:default #+double-samples "libfftw3" #-double-samples "libfftw3f")))

  (unless (find-package :gsll)
    (cffi:define-foreign-library libgslcblas
      (:darwin "libgslcblas.dylib")
      (:unix "libgslcblas.so")
      (:cygwin "cyggslcblas-0.dll")
      (t (:default "libgslcblas")))

    (cffi:define-foreign-library libgsl
      (:darwin "libgsl.dylib")
      (:unix "libgsl.so")
      (:cygwin "cyggsl-0.dll")
      (t (:default "libgsl")))

    (defun load-gsl-library ()
      (cffi:use-foreign-library libgslcblas)
      (cffi:use-foreign-library libgsl))

    (unless (cffi:foreign-library-loaded-p 'libgsl)
      (load-gsl-library)))

  (defmacro def-fftw-fun (name-and-options return-type &body body)
    (let ((prefix #+double-samples "fftw" #-double-samples "fftwf"))
      `(cffi:defcfun (,(format nil "~A_~A" prefix (car name-and-options))
                      ,(cadr name-and-options))
           ,return-type
         ,@body)))

  (defun load-incudine-library ()
    (cffi:use-foreign-library incudine))

  (defun load-fftw-library ()
    (cffi:use-foreign-library fftw))

  (unless (cffi:foreign-library-loaded-p 'incudine) (load-incudine-library))

  (unless (cffi:foreign-library-loaded-p 'fftw) (load-fftw-library))

  (import '(incudine.util:sample incudine.util:+foreign-sample-size+)))

;;; THREADS

(cffi:defcfun "pthread_set_priority" :int
  (thread :pointer)
  (priority :int))

;;; SNDFILE

(cffi:defcfun "sndfile_to_buffer" :void
  (data :pointer)
  (sndfile sf:sndfile)
  (frames :unsigned-long)
  (channels :int)
  (data-offset :unsigned-long)
  (chunk-size :int))

(declaim (inline %map-sndfile-ch-to-buffer))
(cffi:defcfun ("map_sndfile_ch_to_buffer" %map-sndfile-ch-to-buffer) :void
  (data :pointer)
  (sndfile sf:sndfile)
  (frames :unsigned-long)
  (channels :int)
  (buffer-channels :int)
  (data-offset :unsigned-long)
  (chunk-size :int)
  (channel-map-dest :pointer)
  (channel-map-src :pointer)
  (channel-map-size :int))

;;; RING BUFFER

(cffi:defcfun ("copy_from_ring_buffer" %copy-from-ring-buffer) :void
  (dest :pointer)
  (src :pointer)
  (buffer-size :unsigned-long)
  (write-offset :unsigned-long)
  (items :unsigned-long))

(cffi:defcfun ("copy_to_ring_output_buffer" %copy-to-ring-output-buffer) :void
  (dest :pointer)
  (src :pointer)
  (buffer-size :unsigned-long)
  (read-offset :unsigned-long)
  (items :unsigned-long))

;;; MEMORY MANAGEMENT

(declaim (inline %foreign-alloc-sample))
(cffi:defcfun ("foreign_alloc_sample" %foreign-alloc-sample) :pointer
  (size :unsigned-int))

(declaim (inline foreign-alloc-sample))
(defun foreign-alloc-sample (size)
  (let ((ptr (%foreign-alloc-sample size)))
    (if (cffi:null-pointer-p ptr) (error "FOREIGN-ALLOC-SAMPLE failed") ptr)))

(defmacro foreign-realloc-sample (ptr size)
  `(progn
     (cffi:foreign-free ,ptr)
     (setf ,ptr (foreign-alloc-sample ,size))))

(declaim (inline foreign-zero-sample))
(cffi:defcfun "foreign_zero_sample" :pointer
  (ptr :pointer)
  (size :unsigned-int))

(declaim (inline foreign-set))
(cffi:defcfun ("memset" foreign-set) :pointer
  (ptr :pointer)
  (c :int)
  (size :unsigned-int))

(declaim (inline foreign-copy))
(cffi:defcfun ("memcpy" foreign-copy) :void
  (dest :pointer)
  (src :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("init_memory_pool" init-foreign-memory-pool) :unsigned-int
  (size :unsigned-int)
  (ptr :pointer))

(cffi:defcfun ("destroy_memory_pool" destroy-foreign-memory-pool) :void
  (pool :pointer))

(cffi:defcfun ("get_used_size" get-foreign-used-size) :unsigned-int
  (pool :pointer))

(cffi:defcfun ("get_max_size" get-foreign-max-size) :unsigned-int
  (pool :pointer))

(cffi:defcfun ("malloc_ex" foreign-rt-alloc-ex) :pointer
  (size :unsigned-int)
  (pool :pointer))

(cffi:defcfun ("free_ex" foreign-rt-free-ex) :void
  (ptr :pointer)
  (pool :pointer))

(cffi:defcfun ("realloc_ex" foreign-rt-realloc-ex) :pointer
  (ptr :pointer)
  (size :unsigned-int)
  (pool :pointer))

;;; TEMPO

(cffi:defcfun ("tempo_sync" %tempo-sync) sample
  (now :pointer)
  (period sample))

;;; JACK AUDIO DRIVER

#+jack-audio
(progn
  (cffi:defcfun ("ja_initialize" rt-audio-init) :int
    (sample-rate sample)
    (input-channels :unsigned-int)
    (output-channels :unsigned-int)
    (frames-per-buffer :unsigned-int)
    (client-name :pointer))

  (cffi:defcfun ("ja_start" rt-audio-start) :int)

  (cffi:defcfun ("ja_stop" rt-audio-stop) :int)

  (declaim (inline rt-set-io-buffers))
  (cffi:defcfun ("ja_set_lisp_io" rt-set-io-buffers) :void
    (input :pointer)
    (output :pointer))

  (declaim (inline rt-cycle-begin))
  (cffi:defcfun ("ja_cycle_begin" rt-cycle-begin) :uint32)

  (declaim (inline rt-cycle-end))
  (cffi:defcfun ("ja_cycle_end" rt-cycle-end) :void
    (frames :uint32))

  (declaim (inline rt-condition-wait))
  (cffi:defcfun ("ja_condition_wait" rt-condition-wait) :void)

  (declaim (inline rt-transfer-to-c-thread))
  (cffi:defcfun ("ja_transfer_to_c_thread" rt-transfer-to-c-thread) :void)

  (declaim (inline rt-buffer-size))
  (cffi:defcfun ("ja_get_buffer_size" rt-buffer-size) :int)

  (declaim (inline rt-sample-rate))
  (cffi:defcfun ("ja_get_sample_rate" rt-sample-rate) sample)

  (declaim (inline rt-set-busy-state))
  (cffi:defcfun ("ja_set_lisp_busy_state" rt-set-busy-state) :void
    (status :boolean))

  (cffi:defcfun ("ja_get_error_msg" rt-get-error-msg) :string))

;;; PORTAUDIO DRIVER

#+portaudio
(progn
  (cffi:defcfun ("pa_initialize" rt-audio-init) :int
    (sample-rate sample)
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
          incudine.config::*portaudio-output-device* output)))

;;; MOUSE

#+linux
(progn
  (cffi:defcfun "mouse_init" :int)

  (cffi:defcfun "mouse_loop_start" :int (ev :pointer))

  (cffi:defcfun ("mouse_loop_stop" mouse-stop) :int)

  (cffi:defcfun "get_mouse_status" :int))

#-linux
(progn
  (defun mouse-init () 0)
  (defun mouse-stop () 0)
  (defun get-mouse-status () 0))

;;; ANALYSIS

(declaim (inline apply-window))
(cffi:defcfun "apply_window" :void
  (buffer :pointer)
  (window :pointer)
  (window-size :unsigned-long))

(declaim (inline apply-scaled-window))
(cffi:defcfun "apply_scaled_window" :void
  (buffer :pointer)
  (window :pointer)
  (window-size :unsigned-long)
  (mult sample))

(declaim (inline apply-scaled-rectwin))
(cffi:defcfun "apply_scaled_rectwin" :void
  (buffer :pointer)
  (window-size :unsigned-long)
  (mult sample))

(declaim (inline apply-zero-padding))
(cffi:defcfun "apply_zero_padding" :void
  (buffer :pointer)
  (offset :unsigned-long)
  (size :unsigned-long))

(declaim (inline pconv-multiply-partitions))
(cffi:defcfun "pconv_multiply_partitions" :void
  (output :pointer)
  (fdl :pointer)
  (irdata :pointer)
  (fdl-size :unsigned-long)
  (fdl-read-head :unsigned-long)
  (block-size :unsigned-long)
  (partitions :unsigned-long))

;;; COMPLEX TYPES

(cffi:defcstruct sample-complex
  (realpart sample)
  (imagpart sample))

(cffi:defcstruct sample-polar
  (magnitude sample)
  (phase sample))

;; Destructive conversions
(cffi:defcfun "complex_to_polar" :void
  (ptr :pointer)
  (size :unsigned-long))

(cffi:defcfun "polar_to_complex" :void
  (ptr :pointer)
  (size :unsigned-long))

;;; FFT

(def-fftw-fun ("malloc" %foreign-alloc-fft) :pointer
  (size :unsigned-long))

(declaim (inline foreign-alloc-fft))
(defun foreign-alloc-fft (size)
  (declare (type fixnum size))
  (%foreign-alloc-fft (the fixnum (* size +foreign-sample-size+))))

(def-fftw-fun ("free" foreign-free-fft) :void
  (ptr :pointer))

(def-fftw-fun ("plan_dft_r2c_1d" make-fft-plan) :pointer
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :unsigned-int))

(def-fftw-fun ("plan_dft_c2r_1d" make-ifft-plan) :pointer
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :unsigned-int))

(def-fftw-fun ("destroy_plan" fft-destroy-plan) :void
  (plan :pointer))

(def-fftw-fun ("execute_dft_r2c" fft-execute) :void
  (plan :pointer)
  (in :pointer)
  (out :pointer))

(def-fftw-fun ("execute_dft_c2r" ifft-execute) :void
  (plan :pointer)
  (in :pointer)
  (out :pointer))

;;; GSL

(cffi:defcvar ("gsl_rng_mt19937" +gsl-rng-mt19937+ :read-only t) :pointer)

(defstruct (gsl-rng (:constructor %make-gsl-rng) (:copier nil))
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defun make-gsl-rng ()
  (let* ((gen (cffi:foreign-funcall "gsl_rng_alloc"
                                    :pointer +gsl-rng-mt19937+ :pointer))
         (res (%make-gsl-rng :ptr gen)))
    (tg:finalize res (lambda ()
                       (cffi:foreign-funcall "gsl_rng_free" :pointer gen
                                             :void)))
    res))

(defvar *gsl-random-generator* (make-gsl-rng))

(declaim (inline gsl-random-generator))
(defun gsl-random-generator ()
  (gsl-rng-ptr *gsl-random-generator*))

(declaim (inline gsl-seed-random-state))
(defun gsl-seed-random-state (seed)
  (declare (type unsigned-byte seed))
  (cffi:foreign-funcall "gsl_rng_set" :pointer (gsl-random-generator)
                        :unsigned-long seed :pointer))
