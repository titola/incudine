;;; Incudine version of CLM
;;; Copyright (c) 2017 Tito Latini
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

(in-package :cudere-clm)

(define-constant FFTW-FORWARD -1)
(define-constant FFTW-BACKWARD 1)
(define-constant FFTW-ESTIMATE (ash 1 6))

(declaim (inline fftw-plan-dft-1d))
(incudine.external::def-fftw-fun ("plan_dft_1d" fftw-plan-dft-1d) :pointer
  (n :int)
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(declaim (inline fftw-execute))
(incudine.external::def-fftw-fun ("execute" fftw-execute) :void
  (plan :pointer))

(defstruct (fft (:constructor %make-fft) (:copier nil))
  (pointers (cffi:null-pointer) :type foreign-pointer)
  (in-data (cffi:null-pointer) :type foreign-pointer)
  (out-data (cffi:null-pointer) :type foreign-pointer)
  (r-plan (cffi:null-pointer) :type foreign-pointer)
  (i-plan (cffi:null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum))

(defmethod print-object ((obj fft) stream)
  (format stream "#<CLM:FFT :SIZE ~D>" (fft-size obj)))

(defun free-fft-data (pointers)
  (loop for i below 4
        for ptr = (cffi:mem-aref pointers :pointer i) do
          (unless (cffi:null-pointer-p ptr)
            (if (< i 2)
                (incudine.external:foreign-free-fft ptr)
                (incudine.external:fft-destroy-plan ptr)))))

(defun alloc-fft-data (pointers fftsize)
  (setf (cffi:mem-aref pointers :pointer 0)
        (incudine.external:foreign-alloc-fft (* 2 fftsize)))
  (setf (cffi:mem-aref pointers :pointer 1)
        (incudine.external:foreign-alloc-fft (* 2 fftsize)))
  (let ((in-data (cffi:mem-aref pointers :pointer 0))
        (out-data (cffi:mem-aref pointers :pointer 1)))
    (setf (cffi:mem-aref pointers :pointer 2)
          (fftw-plan-dft-1d fftsize in-data out-data FFTW-FORWARD
                            FFTW-ESTIMATE))
    (setf (cffi:mem-aref pointers :pointer 3)
          (fftw-plan-dft-1d fftsize in-data out-data FFTW-BACKWARD
                            FFTW-ESTIMATE)))
  pointers)

(defun free-fft-pointers (pointers)
  (unless (cffi:null-pointer-p pointers)
    (free-fft-data pointers)
    (cffi:foreign-free pointers)))

(defun make-fft (size)
  (declare (type positive-fixnum size))
  (let ((ps (cffi:foreign-alloc :pointer :count 4
                                :initial-element (cffi:null-pointer))))
    (handler-case
        (let ((obj (%make-fft
                     :pointers (alloc-fft-data ps size)
                     :in-data (cffi:mem-aref ps :pointer 0)
                     :out-data (cffi:mem-aref ps :pointer 1)
                     :r-plan (cffi:mem-aref ps :pointer 2)
                     :i-plan (cffi:mem-aref ps :pointer 3)
                     :size size)))
          (incudine-finalize obj (lambda () (free-fft-pointers ps)))
          obj)
      (condition (c)
        (free-fft-pointers ps)
        (error c)))))

(defmethod incudine:free-p ((obj fft))
  (cffi:null-pointer-p (fft-pointers obj)))

(defmethod incudine:free ((obj fft))
  (unless (incudine:free-p obj)
    (free-fft-pointers (fft-pointers obj))
    (setf (fft-pointers obj) (cffi:null-pointer))
    (incudine-cancel-finalization obj)
    (setf (fft-in-data obj) (cffi:null-pointer))
    (setf (fft-out-data obj) (cffi:null-pointer))
    (setf (fft-r-plan obj) (cffi:null-pointer))
    (setf (fft-i-plan obj) (cffi:null-pointer))
    (setf (fft-size obj) 0))
  (values))

(defvar *fft* (make-fft 1024))
(declaim (type fft *fft*))

(defmacro with-pointer-to-fft-data ((real-ptr imag-ptr rdat idat) &body body)
  `(cffi:with-pointer-to-vector-data (,real-ptr ,rdat)
     (cffi:with-pointer-to-vector-data (,imag-ptr ,idat)
       ,@body)))

(defmacro with-fft-slots ((instance struct-name) &body body)
  `(incudine.util::with-struct-slots
       ((pointers in-data out-data r-plan i-plan size) ,instance ,struct-name)
     ,@body))

(declaim (inline update-fft-instance))
(defun update-fft-instance (obj fftsize)
  (with-fft-slots (obj fft)
    (unless (= fftsize size)
      (free-fft-data pointers)
      (alloc-fft-data pointers fftsize)
      (setf in-data (cffi:mem-aref pointers :pointer 0))
      (setf out-data (cffi:mem-aref pointers :pointer 1))
      (setf r-plan (cffi:mem-aref pointers :pointer 2))
      (setf i-plan (cffi:mem-aref pointers :pointer 3))
      (setf size fftsize))
    obj))

(defun fft (rdat idat fftsize &optional (sign 1) (fft-instance *fft*))
  (declare (type (simple-array double-float (*)) rdat idat)
           (type positive-fixnum fftsize) (type (member -1 1) sign)
           (type fft fft-instance))
  (update-fft-instance fft-instance fftsize)
  (with-fft-slots (fft-instance fft)
    (loop for i below (* fftsize 2) by 2
          for j below fftsize do
            (setf (cffi:mem-aref in-data :double i) (aref rdat j))
            (setf (cffi:mem-aref in-data :double (1+ i)) (aref idat j)))
    (fftw-execute (if (= sign -1) r-plan i-plan))
    (loop for i below (* fftsize 2) by 2
          for j below fftsize do
            (setf (aref rdat j) (cffi:mem-aref out-data :double i))
            (setf (aref idat j) (cffi:mem-aref out-data :double (1+ i)))))
  (values))

(defun foreign-fft (rl im fftsize sign fft-instance)
  (declare (type cffi:foreign-pointer rl im)
           (type positive-fixnum fftsize) (type (member -1 1) sign)
           (type fft fft-instance))
  (update-fft-instance fft-instance fftsize)
  (with-fft-slots (fft-instance fft)
    (loop for i below (* fftsize 2) by 2
          for j below fftsize do
            (setf (cffi:mem-aref in-data :double i)
                  (cffi:mem-aref rl :double j))
            (setf (cffi:mem-aref in-data :double (1+ i))
                  (cffi:mem-aref im :double j)))
    (fftw-execute (if (= sign -1) r-plan i-plan))
    (loop for i below (* fftsize 2) by 2
          for j below fftsize do
            (setf (cffi:mem-aref rl :double j)
                  (cffi:mem-aref out-data :double i))
            (setf (cffi:mem-aref im :double j)
                  (cffi:mem-aref out-data :double (1+ i)))))
  (values))

;;; Edited from clm-5/clm.c
(defun rectangular->polar (rdat idat &optional size)
  (declare (type (simple-array double-float (*)) rdat idat))
  (let ((size (or size (length rdat))))
    (declare (type non-negative-fixnum size))
    (dotimes (i size rdat)
      (let ((temp (+ (sqr (aref rdat i)) (sqr (aref idat i)))))
        (if (< temp 1e-8)
            (setf (aref idat i) 0d0 (aref rdat i) 0d0)
            (setf (aref idat i) (- (atan (aref idat i) (aref rdat i)))
                  (aref rdat i) (sqrt temp)))))))

;;; Edited from clm-5/clm.c
(defun rectangular->magnitudes (rdat idat &optional size)
  (declare (type (simple-array double-float (*)) rdat idat))
  (let ((size (or size (length rdat))))
    (declare (type non-negative-fixnum size))
    (dotimes (i size rdat)
      (let ((temp (+ (sqr (aref rdat i)) (sqr (aref idat i)))))
        (setf (aref rdat i) (if (< temp 1e-8) 0d0 (sqrt temp)))))))

;;; Edited from clm-5/clm.c
(defun polar->rectangular (rdat idat)
  (declare (type (simple-array double-float (*)) rdat idat))
  (let ((len (length rdat)))
    (dotimes (i len rdat)
      (let ((mag (aref rdat i))
            (ang (- (aref idat i))))
        (declare (type limited-sample ang))
        (setf (aref rdat i) (* mag (cos ang)))
        (setf (aref idat i) (* mag (sin ang)))))))

;;; Edited from clm-5/mus-lisp
(defun ultraspherical (n x lambda)
  (if (= n 0)
      1.0
      (let ((fn1 (if (= lambda 0.0)
                     (* 2.0 x)
                     (* 2.0 lambda x))))
        (if (= n 1)
            fn1
            (let ((fn 1.0)
                  (fn2 1.0))
              (do ((k 2 (1+ k)))
                  ((> k n) fn)
                (setf fn (/ (- (* 2.0 x (+ k lambda -1.0) fn1)
                               (* (+ k (* 2.0 lambda) -2.0) fn2))
                            k))
                (setf fn2 fn1)
                (setf fn1 fn)))))))

;;; Edited from clm-5/mus.lisp
(defun dolph-chebyshev (type window gamma mu)
  (let* ((N (length window))
	 (alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (freq (/ pi N))
	 (rl (make-double-float-array N))
	 (im (make-double-float-array N)))
    (when (= type ultraspherical-window)
      (cond ((= mu 0.0) (setf type dolph-chebyshev-window))
            ((= mu 1.0) (setf type samaraki-window))))
    (do ((i 0 (1+ i))
	 (phase 0.0 (+ phase freq)))
	((= i N))
      (cond ((= type dolph-chebyshev-window)
             (setf (aref rl i)
                   (double (realpart
                             (cos (* N (acos (* alpha (cos phase)))))))))
            ((= type samaraki-window)
             (setf (aref rl i)
                   (double (realpart
                             (/ (sin (* (+ N 1.0) (acos (* alpha (cos phase)))))
                                (sin (acos (* alpha (cos phase))))))))
             (setf (aref rl i)
                   (double (realpart
                             (ultraspherical N (* alpha (cos phase)) mu)))))))
    (fft rl im N -1)
    (let ((pk 0.0))
      (do ((i 0 (1+ i)))
	  ((= i N))
	(when (> (abs (aref rl i)) pk)
          (setf pk (abs (aref rl i)))))
      (if (and (> pk 0.0) (/= pk 1.0))
	  (let ((scl (/ 1.0 pk)))
	    (do ((i 0 (1+ i)))
		((= i N))
	      (setf (aref rl i) (double (* scl (aref rl i))))))))
    (do ((i 0 (1+ i))
	 (j (/ N 2)))
	((= i N))
      (setf (aref window i) (double (aref rl j)))
      (setf j (+ j 1))
      (if (= j N) (setf j 0)))
    window))

;;; Edited from clm-5/mus.lisp
;;;
;;; References:
;;;
;;;   [1] Fredric J. Harris, "On the Use of Windows for Harmonic Analysis with
;;;   the Discrete Fourier Transform," Proceedings of the IEEE, Vol. 66, No. 1,
;;;   January 1978.
;;;
;;;   [2] Albert H. Nuttall, "Some Windows with Very Good Sidelobe Behaviour",
;;;   IEEE Transactions of Acoustics, Speech, and Signal Processing, Vol.
;;;   ASSP-29, No. 1, February 1981, pp 84-91
;;;
(defun* make-fft-window ((type rectangular-window) (size 32) (beta 0.0) (mu 0.0))
  (declare (type non-negative-fixnum type size) (type real beta mu))
  (let* ((window (make-double-array size))
         (midn (floor size 2))
         (midp1 (/ (1+ size) 2))
         (freq (/ two-pi size))
         (rate (/ 1.0 midn))
         (sr (/ two-pi size))
         (angle 0.0)
         (expn (+ 1.0 (/ (log 2) midn)))
         (expsum 1.0)
         (I0beta (if (= type kaiser-window) (incudine.gen::bessel-i0 beta) 0.0))
         (val 0.0))
    (macrolet ((lw (&body code)
                 `(loop for i from 0 to midn
                        for j = (1- size) then (1- j) do
                          (progn ,.code)
                          (setf (aref window i) (double val))
                          (setf (aref window j) (double val))))
               (lcw (&body code)
                 `(loop for i from 0 to midn
                        for j = (1- size) then (1- j) do
                          (let ((cx (cos angle)))
                            (progn ,.code)
                            (setf (aref window i) (double val))
                            (setf (aref window j) (double val))
                            (incf angle freq)))))
      (cond ((= type rectangular-window)
             (lw (setf val 1.0)))
            ((= type bartlett-window)
             (lw (setf val angle)
                 (incf angle rate)))
            ((= type parzen-window)
             (lw (setf val (- 1.0 (abs (/ (- i midn) midp1))))))
            ((= type welch-window)
             (lw (setf val (- 1.0 (sqr (/ (- i midn) midp1))))))
            ((= type exponential-window)
             (lw (setf val (- expsum 1.0)) (setf expsum (* expsum expn))))
            ((= type kaiser-window)
             (lw (setf val
                       (/ (incudine.gen::bessel-i0
                            (* beta (sqrt (- 1.0 (sqr (/ (- midn i) midn))))))
                          I0beta))))
            ((= type gaussian-window)
             (lw (setf val (exp (* -.5 (sqr (* beta (/ (- midn i) midn))))))))
            ((= type poisson-window)
             (lw (setf val (exp (* (- beta) (/ (- midn i) midn))))))
            ((= type riemann-window)
             (lw (if (= midn i)
                     (setf val 1.0)
                     (setf val (/ (sin (the limited-sample (* sr (- midn i))))
                                  (* sr (- midn i)))))))
            ((= type cauchy-window)
             (lw (setf val (/ 1.0 (+ 1.0 (sqr (/ (* beta (- midn i)) midn)))))))
            ((= type tukey-window)
             (lw (let ((pos (* midn (- 1.0 beta))))
                   (if (>= i pos)
                       (setf val 1.0)
                       (setf val (* .5 (- 1.0 (cos (the limited-sample
                                                     (/ (* pi i) pos))))))))))
            ((= type dolph-chebyshev-window)
             (dolph-chebyshev dolph-chebyshev-window window beta 0.0))
            ((= type samaraki-window)
             (dolph-chebyshev samaraki-window window beta 1.0))
            ((= type ultraspherical-window)
             (dolph-chebyshev ultraspherical-window window beta mu))
            ((= type hann-poisson-window)
             (lcw (setf val (* (- 0.5 (* 0.5 cx))
                               (exp (* (- beta) (/ (- midn i) midn)))))))
            ((= type connes-window)
             (lw (setf val (sqr (- 1.0 (sqr (/ (- i midn) midp1)))))))
            ((= type bartlett-hann-window)
             (lw (setf val (+ 0.62
                              (* -0.48 (abs (- (/ i (1- size)) 0.5)))
                              (* 0.38 (cos (the limited-sample
                                             (* 2 pi (- (/ i (1- size)) 0.5)))))))))
            ((= type bohman-window)
             (lw (let ((r (/ (- midn i) midn)))
                   (setf val (+ (* (- 1.0 r)
                                   (cos (the limited-sample (* pi r))))
                                (* (/ 1.0 pi)
                                   (sin (the limited-sample (* pi r)))))))))
            ((= type flat-top-window)
             (lcw (setf val (+ 0.2156
                               (* -0.4160 cx)
                               (* 0.2781 (cos (* 2 angle)))
                               (* -0.0836 (cos (* 3 angle)))
                               (* 0.0069 (cos (* 4 angle)))))))
            ((= type hann-window)
             (lcw (setf val (- 0.5 (* 0.5 cx)))))
            ((= type rv2-window)
             (lcw (setf val (+ .375
                               (* -0.5 cx)
                               (* .125 (cos (* 2 angle)))))))
            ((= type rv3-window)
             (lcw (setf val (+ (/ 10.0 32.0)
                               (* (/ -15.0 32.0) cx)
                               (* (/ 6.0 32.0) (cos (* 2 angle)))
                               (* (/ -1.0 32.0) (cos (* 3 angle)))))))
            ((= type rv4-window)
             (lcw (setf val (+ (/ 35.0 128.0)
                               (* (/ -56.0 128.0) cx)
                               (* (/ 28.0 128.0) (cos (* 2 angle)))
                               (* (/ -8.0 128.0) (cos (* 3 angle)))
                               (* (/ 1.0 128.0) (cos (* 4 angle)))))))
            ((= type hamming-window)
             (lcw (setf val (- 0.54 (* 0.46 cx)))))
            ((= type blackman2-window)
             (lcw (setf val (* (+ .34401 (* cx (+ -.49755 (* cx .15844))))))))
            ((= type blackman3-window)
             (lcw (setf val (+ .21747
                               (* cx (+ -.45325
                                        (* cx (+ .28256
                                                 (* cx -.04672)))))))))
            ((= type blackman4-window)
             (lcw (setf val
                        (+ .08403
                           (* cx (+ -.29145
                                    (* cx (+ .37569
                                             (* cx (+ -.20762
                                                      (* cx .04119)))))))))))
            ((= type blackman5-window)
             (lcw (setf val (+ .293557
                               (* -.451935 cx)
                               (* .201416 (cos (* 2 angle)))
                               (* -.047926 (cos (* 3 angle)))
                               (* .00502619 (cos (* 4 angle)))
                               (* -.000137555 (cos (* 5 angle)))))))
            ((= type blackman6-window)
             (lcw (setf val (+ .271220
                               (* -.433444 cx)
                               (* .218004 (cos (* 2 angle)))
                               (* -.065785 (cos (* 3 angle)))
                               (* .01076186 (cos (* 4 angle)))
                               (* -.000770012 (cos (* 5 angle)))
                               (* .0000136808 (cos (* 6 angle)))))))
            ((= type blackman7-window)
             (lcw (setf val (+ .253317
                               (* -.416327 cx)
                               (* .228839 (cos (* 2 angle)))
                               (* -.081575 (cos (* 3 angle)))
                               (* .01773592 (cos (* 4 angle)))
                               (* -.002096702 (cos (* 5 angle)))
                               (* .0001067741 (cos (* 6 angle)))
                               (* -.0000012807(cos (* 7 angle)))))))
            ((= type blackman8-window)
             (lcw (setf val (+ .238433
                               (* -.400554 cx)
                               (* .235824 (cos (* 2 angle)))
                               (* -.095279 (cos (* 3 angle)))
                               (* .02537395 (cos (* 4 angle)))
                               (* -.00415243  (cos (* 5 angle)))
                               (* .0003685604 (cos (* 6 angle)))
                               (* -.0000138435 (cos (* 7 angle)))
                               (* .000000116180(cos (* 8 angle)))))))
            ((= type blackman9-window)
             (lcw (setf val (+ .225734
                               (* -.386012 cx)
                               (* .240129 (cos (* 2 angle)))
                               (* -.107054 (cos (* 3 angle)))
                               (* .03325916 (cos (* 4 angle)))
                               (* -.00687337  (cos (* 5 angle)))
                               (* .0008751673 (cos (* 6 angle)))
                               (* -.0000600859 (cos (* 7 angle)))
                               (* .000001710716 (cos (* 8 angle)))
                               (* -.00000001027272(cos (* 9 angle)))))))
            ((= type blackman10-window)
             (lcw (setf val (+ .215153
                               (* -.373135 cx)
                               (* .242424 (cos (* 2 angle)))
                               (* -.1166907 (cos (* 3 angle)))
                               (* .04077422 (cos (* 4 angle)))
                               (* -.01000904 (cos (* 5 angle)))
                               (* .0016398069 (cos (* 6 angle)))
                               (* -.0001651660 (cos (* 7 angle)))
                               (* .000008884663 (cos (* 8 angle)))
                               (* -.000000193817 (cos (* 9 angle)))
                               (* .000000000848248 (cos (* 10 angle))))))))
      window)))

(defun apply-window (rdat window)
  (declare (type (simple-array double-float (*)) rdat window))
  (let ((len (min (length rdat) (length window))))
    (declare (type positive-fixnum len))
    (dotimes (i len rdat)
      (setf (aref rdat i) (* (aref rdat i) (aref window i))))))

;;; Edited from clm-5/mus.lisp
(defun spectrum (rdat idat window &optional (type 0))
  (declare (type (simple-array double-float (*)) rdat idat)
           (type (or (simple-array double-float (*)) null) window)
           (type (mod 2) type))
  (let* ((len (length rdat))
	 (len2 (floor len 2)))
    (when window
      (apply-window rdat window))
    (clear-array idat)
    (fft rdat idat len 1)
    (let ((maxa 0.0)
	  (lowest 1.0e-6))
      (dotimes (i len)
        (setf (aref rdat i)
              (double (sqrt (+ (sqr (max lowest (aref rdat i)))
                               (sqr (max lowest (aref idat i)))))))
        (setf maxa (max maxa (abs (aref rdat i)))))
      (when (plusp maxa)
        (dotimes (i len2)
          (setf (aref rdat i)
                (double
                  (if (= type 0)
                      (* (/ 20 (log 10))
                         (log (max (/ (aref rdat i) maxa) lowest)))
                      (/ (aref rdat i) maxa))))))
      rdat)))

(declaim (inline convolution-loop))
(defun convolution-loop (rl1 rl2 n)
  (macrolet ((ref (ptr i) `(cffi:mem-aref ,ptr :double ,i)))
    (setf (ref rl1 0) (/ (* (ref rl1 0) (ref rl2 0)) n))
    (setf (ref rl2 0) 0d0)
    (loop for i of-type fixnum from 1 to (ash n -1)
          for nn2 of-type fixnum = (- n i)
          with invn = (/ .25 n) do
            (let ((rep (+ (ref rl1 i) (ref rl1 nn2)))
                  (rem (- (ref rl1 i) (ref rl1 nn2)))
                  (aip (+ (ref rl2 i) (ref rl2 nn2)))
                  (aim (- (ref rl2 i) (ref rl2 nn2))))
              (setf (ref rl1 i) (* invn (+ (* rep aip) (* aim rem))))
              (setf (ref rl2 i) (* invn (- (* aim aip) (* rep rem))))
              (setf (ref rl1 nn2) (ref rl1 i))
              (setf (ref rl2 nn2) (- (ref rl2 i)))))))

;;; Edited from clm-5/clm.c
(defun convolution (rdat idat fftsize &optional ignored (fft-instance *fft*))
  (declare (type (simple-array double-float (*)) rdat idat)
           (type positive-fixnum fftsize) (type fft fft-instance)
           (ignore ignored))
  (fft rdat idat fftsize 1 fft-instance)
  (with-pointer-to-fft-data (rl1 rl2 rdat idat)
    (convolution-loop rl1 rl2 fftsize))
  (fft rdat idat fftsize -1 fft-instance)
  rdat)

(declaim (inline foreign-convolution))
(defun foreign-convolution (rl1 rl2 n fft-instance)
  (foreign-fft rl1 rl2 n 1 fft-instance)
  (convolution-loop rl1 rl2 n)
  (foreign-fft rl1 rl2 n -1 fft-instance))

(defmacro with-foreign-double-array ((var size) &body body)
  `(cffi:with-foreign-pointer
       (,var (the positive-fixnum
               (* ,size #.(cffi:foreign-type-size :double))))
     ,@body))

;;; Edited from clm-5/clm.c
(defun autocorrelate (data size &optional (fft-instance *fft*))
  (declare (type (simple-array double-float (*)) data)
           (type positive-fixnum size) (type fft fft-instance))
  (let ((n2 (ash size -1))
        (rsize (/ 1.0 size)))
    (cffi:with-pointer-to-vector-data (rl data)
      (with-foreign-double-array (im size)
        (foreign-fft rl im size 1 fft-instance)
        (loop for i below size do
                (setf (cffi:mem-aref rl :double i)
                      (+ (sqr (cffi:mem-aref rl :double i))
                         (sqr (cffi:mem-aref im :double i)))))
        (incudine.external::foreign-zero-sample im size)
        (foreign-fft rl im size -1 fft-instance)
        (loop for i to n2 do
                (setf (cffi:mem-aref rl :double i)
                      (* (cffi:mem-aref rl :double i) rsize)))
        (loop for i from (1+ n2) below size do
                (setf (cffi:mem-aref rl :double i) 0d0))))
    data))

;;; Edited from clm-5/clm.c
(defun correlate (data1 data2 size &optional (fft-instance *fft*))
  (declare (type (simple-array double-float (*)) data1 data2)
           (type positive-fixnum size) (type fft fft-instance))
  (let ((rsize (/ 1.0 size)))
    (with-foreign-double-array (im1 size)
      (with-foreign-double-array (im2 size)
        (with-pointer-to-fft-data (rl1 rl2 data1 data2)
          (foreign-fft rl1 im1 size 1 fft-instance)
          (foreign-fft rl2 im2 size 1 fft-instance)
          (dotimes (i size)
            (let ((tmp1 (* (aref data1 i) (aref data2 i)))
                  (tmp2 (* (cffi:mem-aref im1 :double i)
                           (cffi:mem-aref im2 :double i)))
                  (tmp3 (* (aref data1 i) (cffi:mem-aref im2 :double i)))
                  (tmp4 (* (aref data2 i) (cffi:mem-aref im1 :double i))))
              (setf (cffi:mem-aref rl1 :double i) (+ tmp1 tmp2))
              (setf (cffi:mem-aref im1 :double i) (- tmp3 tmp4))))
          (foreign-fft rl1 im1 size -1 fft-instance)
          (dotimes (i size data1)
            (setf (cffi:mem-aref rl1 :double i)
                  (* (cffi:mem-aref rl1 :double i) rsize))))))))
