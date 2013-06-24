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

(in-package :incudine.external)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library (incudine :search-path #.(load-time-value
                                                         (or *compile-file-pathname*
                                                             *load-pathname*)))
    (:unix "libincudine.so")
    (:darwin "libincudine.dylib")
    (:cygwin "cygincudine-0.dll")
    (t (:default "libincudine")))

  (cffi:define-foreign-library fftw
    (:unix #.(if (eq incudine.util:*sample-type* 'double-float)
                 "libfftw3.so"
                 "libfftw3f.so"))
    (:darwin #.(if (eq incudine.util:*sample-type* 'double-float)
                   "libfftw3.dylib"
                   "libfftw3f.dylib"))
    (:cygwin #.(if (eq incudine.util:*sample-type* 'double-float)
                   "cygfftw3-0.dll"
                   "cygfftw3f-0.dll"))
    (t (:default #.(if (eq incudine.util:*sample-type* 'double-float)
                       "libfftw3"
                       "libfftw3f"))))

  (unless (find-package :gsll)
    (cffi:define-foreign-library libgslcblas
      (:unix "libgslcblas.so")
      (:darwin "libgslcblas.dylib")
      (:cygwin "cyggslcblas-0.dll")
      (t (:default "libgslcblas")))

    (cffi:define-foreign-library libgsl
      (:unix "libgsl.so")
      (:darwin "libgsl.dylib")
      (:cygwin "cyggsl-0.dll")
      (t (:default "libgsl")))

    (defun load-gsl-library ()
      (cffi:use-foreign-library libgslcblas)
      (cffi:use-foreign-library libgsl))

    (unless (cffi:foreign-library-loaded-p 'libgsl)
      (load-gsl-library)))

  (defmacro def-fftw-fun (name-and-options return-type &body body)
    (let ((prefix (if (eq incudine.util:*sample-type* 'double-float)
                      "fftw" "fftwf")))
      `(cffi:defcfun (,(format nil "~A_~A" prefix (car name-and-options))
                      ,(cadr name-and-options))
           ,return-type
         ,@body)))

  (defun load-incudine-library ()
    (cffi:use-foreign-library incudine))

  (defun load-fftw-library ()
    (cffi:use-foreign-library fftw))

  (unless (cffi:foreign-library-loaded-p 'incudine)
    (load-incudine-library))
  (unless (cffi:foreign-library-loaded-p 'fftw)
    (load-fftw-library))

  (import '(incudine.util:sample incudine.util:+foreign-sample-size+)))

;;; HASH-TABLE

(cffi:defcfun "int_hash" :int32
  "Hash function for integer 32bit."
  (in-key :int32))

;;; THREADS

(cffi:defcfun "pthread_set_priority" :int
  (thread   :pointer)
  (priority :int))

;;; SNDFILE

(cffi:defcfun "sndfile_to_buffer" :void
  (data        :pointer)
  (sndfile     sf:sndfile)
  (frames      :unsigned-long)
  (channels    :int)
  (data-offset :unsigned-long)
  (chunk-size  :int))

(declaim (inline %map-sndfile-ch-to-buffer))
(cffi:defcfun ("map_sndfile_ch_to_buffer" %map-sndfile-ch-to-buffer) :void
  (data             :pointer)
  (sndfile          sf:sndfile)
  (frames           :unsigned-long)
  (channels         :int)
  (buffer-channels  :int)
  (data-offset      :unsigned-long)
  (chunk-size       :int)
  (channel-map-dest :pointer)
  (channel-map-src  :pointer)
  (channel-map-size :int))

;;; RING BUFFER

(cffi:defcfun ("copy_from_ring_buffer" %copy-from-ring-buffer) :void
  (dest         :pointer)
  (src          :pointer)
  (buffer-size  :unsigned-long)
  (write-offset :unsigned-long)
  (items        :unsigned-long))

(cffi:defcfun ("copy_to_ring_output_buffer" %copy-to-ring-output-buffer) :void
  (dest         :pointer)
  (src          :pointer)
  (buffer-size  :unsigned-long)
  (read-offset  :unsigned-long)
  (items        :unsigned-long))

;;; MEMORY MANAGEMENT

(declaim (inline %foreign-alloc-sample))
(cffi:defcfun ("foreign_alloc_sample" %foreign-alloc-sample) :pointer
  (size :unsigned-int))

(declaim (inline foreign-alloc-sample))
(defun foreign-alloc-sample (size)
  (let ((ptr (%foreign-alloc-sample size)))
    (if (cffi:null-pointer-p ptr)
        (error "FOREIGN-ALLOC-SAMPLE failed")
        ptr)))

(defmacro foreign-realloc-sample (ptr size)
  `(progn
     (cffi:foreign-free ,ptr)
     (setf ,ptr (foreign-alloc-sample ,size))))

(cffi:defcfun "foreign_zero_sample" :pointer
  (ptr  :pointer)
  (size :unsigned-int))

(cffi:defcfun ("memset" foreign-set) :pointer
  (ptr  :pointer)
  (c    :int)
  (size :unsigned-int))

(cffi:defcfun ("memcpy" foreign-copy) :void
  (dest  :pointer)
  (src   :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("init_memory_pool" init-foreign-memory-pool) :unsigned-int
  (size :unsigned-int)
  (ptr  :pointer))

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
  (ptr  :pointer)
  (pool :pointer))

(cffi:defcfun ("realloc_ex" foreign-rt-realloc-ex) :pointer
  (ptr  :pointer)
  (size :unsigned-int)
  (pool :pointer))

;;; TEMPO

(cffi:defcfun ("tempo_sync" %tempo-sync) sample
  (now    :pointer)
  (period sample))

;;; PORTAUDIO DRIVER

(cffi:defcfun ("pa_initialize" rt-audio-init) :int
  (sample-rate       sample)
  (input-channels    :unsigned-int)
  (output-channels   :unsigned-int)
  (frames-per-buffer :unsigned-int)
  (client-name       :string))

(cffi:defcfun ("pa_start" rt-audio-start) :int)

(cffi:defcfun ("pa_stop" rt-audio-stop) :int)

(declaim (inline rt-get-input))
(cffi:defcfun ("pa_get_input" rt-get-input) :void
  (inputs :pointer))

(declaim (inline rt-set-output))
(cffi:defcfun ("pa_set_output" rt-set-output) :void
  (outputs :pointer))

(declaim (inline rt-condition-wait))
(cffi:defcfun ("pa_condition_wait" rt-condition-wait) :int)

(cffi:defcfun ("pa_get_error_msg" rt-get-error-msg) :string)

;;; MOUSE

#+linux
(progn
  (cffi:defcfun "mouse_init" :int)

  (cffi:defcfun "mouse_loop_start" :int
    (ev :pointer))

  (cffi:defcfun ("mouse_loop_stop" mouse-stop) :int)

  (cffi:defcfun "get_mouse_status" :int))

#-linux
(progn
  (defun mouse-init () 0)
  (defun mouse-stop () 0)
  (defun get-mouse-status () 0))

;;; ANALYSIS

(cffi:defcfun "apply_window" :void
  (buffer      :pointer)
  (window      :pointer)
  (window-size :unsigned-long))

(cffi:defcfun "apply_scaled_window" :void
  (buffer      :pointer)
  (window      :pointer)
  (window-size :unsigned-long)
  (size        :unsigned-long))

(cffi:defcfun "apply_zero_padding" :void
  (buffer :pointer)
  (offset :unsigned-long)
  (size   :unsigned-long))

;;; COMPLEX TYPES

(cffi:defcstruct sample-complex
  (realpart sample)
  (imagpart sample))

(cffi:defcstruct sample-polar
  (magnitude sample)
  (phase     sample))

;; Destructive conversions
(cffi:defcfun "complex_to_polar" :void
  (ptr  :pointer)
  (size :unsigned-long))

(cffi:defcfun "polar_to_complex" :void
  (ptr  :pointer)
  (size :unsigned-long))

;;; FFT

(def-fftw-fun ("malloc" %foreign-alloc-fft) :pointer
  (size :unsigned-long))

(defun foreign-alloc-fft (size)
  (%foreign-alloc-fft (* size +foreign-sample-size+)))

(def-fftw-fun ("free" foreign-free-fft) :void
  (ptr :pointer))

(def-fftw-fun ("plan_dft_r2c_1d" make-fft-plan) :pointer
  (n     :int)
  (in    :pointer)
  (out   :pointer)
  (flags :unsigned-int))

(def-fftw-fun ("plan_dft_c2r_1d" make-ifft-plan) :pointer
  (n     :int)
  (in    :pointer)
  (out   :pointer)
  (flags :unsigned-int))

(def-fftw-fun ("execute" fftw-execute) :void
  (plan :pointer))

(def-fftw-fun ("destroy_plan" fft-destroy-plan) :void
  (plan :pointer))

;;; GSL

(cffi:defcvar ("gsl_rng_mt19937" +gsl-rng-mt19937+ :read-only t) :pointer)

(defstruct (gsl-rng (:constructor %make-gsl-rng)
                    (:copier nil))
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defun make-gsl-rng ()
  (let* ((gen (cffi:foreign-funcall "gsl_rng_alloc"
                                    :pointer +gsl-rng-mt19937+ :pointer))
         (res (%make-gsl-rng :ptr gen)))
    (tg:finalize res (lambda ()
                       (cffi:foreign-funcall "gsl_rng_free"
                                             :pointer gen :void)))
    res))

(defvar *gsl-random-generator* (make-gsl-rng))

(declaim (inline gsl-random-generator))
(defun gsl-random-generator ()
  (gsl-rng-ptr *gsl-random-generator*))

(declaim (inline gsl-seed-random-state))
(defun gsl-seed-random-state (seed)
  (declare (type unsigned-byte seed))
  (cffi:foreign-funcall "gsl_rng_set"
                        :pointer (gsl-random-generator)
                        :unsigned-long seed :pointer))
