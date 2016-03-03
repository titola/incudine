;;; Test for alien stack alignment with SBCL on x86 (no darwin).

(require :cffi)

(cffi:define-foreign-library fftw
  (:unix "libfftw3.so")
  (:cygwin "cygfftw3-0.dll")
  (t (:default "libfftw3")))

(cffi:use-foreign-library fftw)

(macrolet ((with-fftw-pointer ((var size) &body body)
	     `(let ((,var (cffi:foreign-funcall "fftw_malloc"
                            :unsigned-int (* 16 (1+ (ash ,size -1)))
			    :pointer)))
		(unwind-protect (progn ,@body)
		  (cffi:foreign-funcall "fftw_free" :pointer ,var :void)))))
  (let ((aligned-p t)
        (size 128))
    (with-fftw-pointer (in size)
      (with-fftw-pointer (out size)
	(handler-case
	    (let ((plan (cffi:foreign-funcall "fftw_plan_dft_r2c_1d"
                          :int size :pointer in :pointer out
			  :unsigned-int 0 :pointer)))
	      (cffi:foreign-funcall "fftw_destroy_plan" :pointer plan :void))
	  (error () (setf aligned-p nil)))))
    (with-open-file (f (second sb-ext:*posix-argv*)
                     :direction :output
                     :if-exists :supersede)
      (format f "~
;;; Delete this automatically generated file to repeat the stack align test.
(in-package :incudine.analysis)
~:[(pushnew :FFTW-NO-SIMD *features*)~;~]
(define-constant +fftw-no-simd+ ~D)
(define-constant +fftw-measure+ +fftw-no-simd+)
(define-constant +fftw-patient+ (logior (ash 1 5) +fftw-no-simd+))
(define-constant +fftw-estimate+ (logior (ash 1 6) +fftw-no-simd+))~%"
                aligned-p (if aligned-p 0 (ash 1 17))))))
