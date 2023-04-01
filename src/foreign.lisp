;;; Copyright (c) 2013-2023 Tito Latini
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
  (cffi:define-foreign-library fftw
    (:darwin #+double-samples "libfftw3.dylib" #-double-samples "libfftw3f.dylib")
    (:unix #+double-samples "libfftw3.so" #-double-samples "libfftw3f.so")
    (:cygwin #+double-samples "cygfftw3-3.dll" #-double-samples "cygfftw3f-3.dll")
    (:win32 #+double-samples "libfftw3-3.dll" #-double-samples "libfftw3f-3.dll")
    (t (:default #+double-samples "libfftw3" #-double-samples "libfftw3f")))

  (unless (find-package :gsll)
    (cffi:define-foreign-library libgslcblas
      (:darwin "libgslcblas.dylib")
      (:unix "libgslcblas.so")
      (:cygwin "cyggslcblas-0.dll")
      (:win32 "libgslcblas-0")
      (t (:default "libgslcblas")))

    (cffi:define-foreign-library libgsl
      (:darwin "libgsl.dylib")
      (:unix "libgsl.so")
      (:cygwin "cyggsl-27.dll")
      (:win32 "libgsl-27.dll")
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

  (defun load-fftw-library ()
    (cffi:use-foreign-library fftw))

  (unless (cffi:foreign-library-loaded-p 'fftw) (load-fftw-library))

  (import '(incudine.util:sample incudine.util:+foreign-sample-size+)))

;;; ERRORS

(cffi:defcvar "errno" :int)

(defun errno-to-string (&optional (errno *errno*))
  "Return a string that describes the error code ERRNO.

ERRNO defaults to the foreign integer variable errno. See errno man
page for details."
  (cffi:foreign-funcall "strerror" :int errno :string))

;;; THREADS

#-win32
(cffi:defcfun ("pthread_priority" thread-priority) :int
  (thread :pointer))

#-win32
(cffi:defcfun ("pthread_set_priority" thread-set-priority) :int
  (thread :pointer)
  (priority :int))

#-win32
(cffi:defcfun "pthread_getaffinity_np" :int
  (thread :pointer)
  (size :unsigned-int)
  (cpuset :pointer))

#-win32
(cffi:defcfun "pthread_setaffinity_np" :int
  (thread :pointer)
  (size :unsigned-int)
  (cpuset :pointer))

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
  "Allocate enough foreign memory to hold SIZE samples."
  (let ((ptr (%foreign-alloc-sample size)))
    (if (cffi:null-pointer-p ptr)
        (incudine::foreign-alloc-error "FOREIGN-ALLOC-SAMPLE failed.")
        ptr)))

(declaim (inline %foreign-realloc))
(cffi:defcfun ("realloc" %foreign-realloc) :pointer
  (ptr :pointer)
  (size :unsigned-int))

(declaim (inline foreign-zero-sample))
(cffi:defcfun "foreign_zero_sample" :pointer
  "Set the first SIZE samples of the area starting at PTR to zero."
  (ptr :pointer)
  (size :unsigned-int))

(declaim (inline foreign-set))
(cffi:defcfun ("memset" foreign-set) :pointer
  "Fills the first SIZE bytes of the foreign memory area pointed to by
PTR with the byte C.

Return the pointer to the memory area PTR."
  (ptr :pointer)
  (c :int)
  (size :unsigned-int))

(declaim (inline foreign-copy))
(cffi:defcfun ("memcpy" foreign-copy) :void
  "Copy BYTES bytes from foreign memory area SRC to foreign memory
area DEST. The memory areas must not overlap."
  (dest :pointer)
  (src :pointer)
  (bytes :unsigned-int))

(declaim (inline foreign-copy-samples))
(defun foreign-copy-samples (dest src size)
  "Copy SIZE samples from foreign memory area SRC to foreign memory
area DEST. The memory areas must not overlap."
  (declare (type cffi:foreign-pointer dest src)
           (type alexandria:non-negative-fixnum size))
  (foreign-copy dest src (the alexandria:non-negative-fixnum
                           (* size +foreign-sample-size+))))

(cffi:defcfun ("init_memory_pool" init-foreign-memory-pool) :unsigned-int
  "Initialize the foreign memory pool of size SIZE pointed to by PTR.

Example:

    ;; Pool size of 10 MB
    (define-constant +mem-pool-size+ (* 10 1024 1024))

    (defvar *mem-pool* (cffi:foreign-alloc :char :count +mem-pool-size+))
    (declaim (type foreign-pointer *mem-pool*))

    (incudine.external:init-foreign-memory-pool +mem-pool-size+ *mem-pool*)"
  (size :unsigned-int)
  (ptr :pointer))

(cffi:defcfun ("destroy_memory_pool" destroy-foreign-memory-pool) :void
  "Destroy the foreign memory pool pointed to by POOL."
  (pool :pointer))

(cffi:defcfun ("get_used_size" get-foreign-used-size) :unsigned-int
  "Return the number of bytes allocated from the foreign memory POOL."
  (pool :pointer))

(cffi:defcfun ("get_max_size" get-foreign-max-size) :unsigned-int
  "Return the maximum size of the foreign memory POOL."
  (pool :pointer))

(cffi:defcfun ("malloc_ex" foreign-alloc-ex) :pointer
  "Allocate SIZE bytes from the foreign memory POOL and return a
pointer to the allocated memory."
  (size :unsigned-int)
  (pool :pointer))

(cffi:defcfun ("free_ex" foreign-free-ex) :void
  "Free PTR previously allocated by FOREIGN-ALLOC-EX."
  (ptr :pointer)
  (pool :pointer))

(cffi:defcfun ("realloc_ex" foreign-realloc-ex) :pointer
  "Change the size of the foreign memory block of POOL to SIZE bytes.

Return a pointer to the newly allocated memory."
  (ptr :pointer)
  (size :unsigned-int)
  (pool :pointer))

;;; TEMPO

(cffi:defcfun ("tempo_sync" %tempo-sync) sample
  (now :pointer)
  (period sample))

;;; MOUSE

#+x11
(progn
  (cffi:defcfun "mouse_init" :int)

  (cffi:defcfun "mouse_loop_start" :int (ev :pointer))

  (cffi:defcfun "mouse_loop_stop" :int)

  (cffi:defcfun "get_mouse_status" :int))

#-x11
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

(cffi:defcfun "complex_to_polar" :void
  "Convert the representation of the complex sample data from complex
to polar.

PTR is the foreign pointer to SIZE complex samples."
  (ptr :pointer)
  (size :unsigned-long))

(cffi:defcfun "polar_to_complex" :void
  "Convert the representation of the complex sample data from polar
to complex.

PTR is the foreign pointer to SIZE complex samples."
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
    (sb-ext:finalize res
      (lambda () (cffi:foreign-funcall "gsl_rng_free" :pointer gen :void))
      :dont-save t)
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

;;; MISC

(declaim (inline qsort))
(cffi:defcfun "qsort" :void
  (base :pointer)
  (nmemb :unsigned-int)
  (size :unsigned-int)
  (compar :pointer))

(cffi:defcallback sample-cmp :int ((a :pointer) (b :pointer))
  (let ((x (cffi:mem-ref a 'sample))
        (y (cffi:mem-ref b 'sample)))
    (cond ((> x y) 1)
          ((< x y) -1)
          (t 0))))

(declaim (inline %fmod))
(cffi:defcfun ("fmod" %fmod) :double
  (x :double)
  (y :double))

(declaim (inline %pow))
(cffi:defcfun ("pow" %pow) :double
  (x :double)
  (y :double))

(declaim (inline incudine.util:pow))
(defun incudine.util:pow (base power)
  "Return BASE raised to the POWER.
The returned value is of type DOUBLE-FLOAT.

If BASE is a negative value and POWER is not an integer,
FLOATING-POINT-INVALID-OPERATION is signalled."
  (%pow (coerce base 'double-float) (coerce power 'double-float)))

(declaim (inline %exp))
(cffi:defcfun ("exp" %exp) :double (x :double))
