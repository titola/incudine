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

(in-package :incudine.analysis)

;;; Inspired by the UAna object in Ge Wang and Perry Cook's ChucK

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:incudine-missing-arg
     incudine:smp-ref
     incudine:circular-shift
     incudine:free
     incudine:free-p
     incudine:incudine-finalize
     incudine:incudine-cancel-finalization
     incudine:now))
  #+fftw-no-simd
  (format *error-output* "~%WARNING: using planner flag FFTW_NO_SIMD~%"))

(defstruct (ring-buffer (:include incudine-object)
                        (:copier nil))
  (data (null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (head 0 :type non-negative-fixnum)
  (mask 0 :type non-negative-fixnum)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(defmethod print-object ((obj ring-buffer) stream)
  (format stream "#<~A size ~D head ~D>" (type-of obj) (ring-buffer-size obj)
          (ring-buffer-head obj)))

(defstruct (ring-input-buffer (:include ring-buffer)
                              (:constructor %make-ring-input-buffer)
                              (:copier nil)))

(defstruct (ring-output-buffer (:include ring-buffer)
                               (:constructor %make-ring-output-buffer)
                               (:copier nil))
  ;; The slot TMP is added to avoid consing in RING-OUTPUT-BUFFER-NEXT
  (tmp (null-pointer) :type foreign-pointer))

(define-constant +ring-buffer-pool-initial-size+ 200)

(defvar *ring-input-buffer-pool*
  (make-incudine-object-pool +ring-buffer-pool-initial-size+ #'%make-ring-input-buffer nil))
(declaim (type incudine-object-pool *ring-input-buffer-pool*))

(defvar *rt-ring-input-buffer-pool*
  (make-incudine-object-pool +ring-buffer-pool-initial-size+ #'%make-ring-input-buffer t))
(declaim (type incudine-object-pool *rt-ring-input-buffer-pool*))

(defvar *ring-output-buffer-pool*
  (make-incudine-object-pool +ring-buffer-pool-initial-size+ #'%make-ring-output-buffer nil))
(declaim (type incudine-object-pool *ring-output-buffer-pool*))

(defvar *rt-ring-output-buffer-pool*
  (make-incudine-object-pool +ring-buffer-pool-initial-size+ #'%make-ring-output-buffer t))
(declaim (type incudine-object-pool *rt-ring-output-buffer-pool*))

(defvar *dummy-ring-buffer* (make-ring-buffer))
(declaim (type ring-buffer *dummy-ring-buffer*))

(defun make-ring-input-buffer (bufsize
                               &optional (real-time-p (incudine.util:allow-rt-memory-p)))
  (declare (type non-negative-fixnum bufsize))
  (let ((rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
        (bufsize (incudine.util:next-power-of-two bufsize)))
    (multiple-value-bind (data-ptr buf free-fn pool)
        (if rt-p
            (values (foreign-rt-alloc 'sample :count bufsize :zero-p t)
                    (incudine.util::alloc-rt-object *rt-ring-input-buffer-pool*)
                    #'safe-foreign-rt-free
                    *rt-ring-input-buffer-pool*)
            (values (foreign-alloc-sample bufsize)
                    (incudine.util::alloc-object *ring-input-buffer-pool*)
                    #'foreign-free
                    *ring-input-buffer-pool*))
      (declare (type foreign-pointer data-ptr) (type ring-input-buffer buf)
               (type incudine-object-pool pool))
      (handler-case
          (incudine.util::with-struct-slots
              ((data size mask real-time-p foreign-free)
               buf ring-input-buffer "INCUDINE.ANALYSIS")
            (setf data data-ptr
                  size bufsize
                  mask (1- bufsize)
                  real-time-p rt-p
                  foreign-free free-fn))
        (condition (c)
          (funcall free-fn data-ptr)
          (incudine-object-pool-expand pool 1)
          (error c)))
      (incudine-finalize buf
        (lambda ()
          (funcall free-fn data-ptr)
          (incudine-object-pool-expand pool 1))))))

(defun make-ring-output-buffer (bufsize
                                &optional (real-time-p (incudine.util:allow-rt-memory-p)))
  (declare (type non-negative-fixnum bufsize))
  (flet ((foreign-alloc (bufsize zero-p rt-p)
           (if rt-p
               (foreign-rt-alloc 'sample :count bufsize :zero-p zero-p)
               (foreign-alloc-sample bufsize))))
    (let ((rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
          (bufsize (incudine.util:next-power-of-two bufsize))
          (data-ptr nil)
          (tmp-ptr nil))
      (multiple-value-bind (buf free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-ring-output-buffer-pool*)
                      #'safe-foreign-rt-free
                      *rt-ring-output-buffer-pool*)
              (values (incudine.util::alloc-object *ring-output-buffer-pool*)
                      #'foreign-free
                      *ring-output-buffer-pool*))
        (declare (type ring-output-buffer buf) (type incudine-object-pool pool))
        (handler-case
            (progn
              (setf data-ptr (foreign-alloc bufsize t rt-p))
              (setf tmp-ptr (foreign-alloc bufsize nil rt-p))
              (incudine.util::with-struct-slots
                  ((data tmp size mask real-time-p foreign-free)
                   buf ring-output-buffer "INCUDINE.ANALYSIS")
                (setf data data-ptr
                      tmp tmp-ptr
                      size bufsize
                      mask (1- bufsize)
                      real-time-p rt-p
                      foreign-free free-fn)))
          (condition (c)
            (dolist (p (list data-ptr tmp-ptr))
              (if p (funcall free-fn p)))
            (incudine-object-pool-expand pool 1)
            (error c)))
        (incudine-finalize buf
          (lambda ()
            (funcall free-fn data-ptr)
            (funcall free-fn tmp-ptr)
            (incudine-object-pool-expand pool 1)))))))

(defmethod free-p ((obj ring-buffer))
  (zerop (ring-buffer-size obj)))

(defmethod free ((obj ring-buffer))
  (unless (free-p obj)
    (let ((foreign-free (ring-buffer-foreign-free obj))
          (outbuf-p (ring-output-buffer-p obj)))
      (funcall foreign-free #1=(ring-buffer-data obj))
      (setf (ring-buffer-size obj) 0)
      (setf #1# (null-pointer))
      (when outbuf-p
        (funcall foreign-free #2=(ring-output-buffer-tmp obj))
        (setf #2# (null-pointer)))
      (incudine-cancel-finalization obj)
      (setf (ring-buffer-head obj) 0)
      (if (ring-buffer-real-time-p obj)
          (incudine.util::free-rt-object obj
            (if outbuf-p
              *rt-ring-output-buffer-pool*
              *rt-ring-input-buffer-pool*))
          (incudine.util::free-object obj
            (if outbuf-p
                *ring-output-buffer-pool*
                *ring-input-buffer-pool*)))
      (incudine.util:nrt-msg debug "Free ~A" (type-of obj))))
  (values))

(declaim (inline ring-input-buffer-put))
(defun ring-input-buffer-put (value buf)
  (declare (type sample value) (type ring-input-buffer buf))
  (setf (smp-ref (ring-input-buffer-data buf) (ring-input-buffer-head buf))
        value)
  (setf (ring-input-buffer-head buf)
        (logand (1+ (ring-input-buffer-head buf)) (ring-buffer-mask buf)))
  value)

(declaim (inline copy-from-ring-buffer))
(defun copy-from-ring-buffer (foreign-array ring-buffer items)
  (declare (type ring-buffer ring-buffer) (type foreign-pointer foreign-array)
           (type non-negative-fixnum items))
  (%copy-from-ring-buffer foreign-array (ring-buffer-data ring-buffer)
                          (ring-buffer-size ring-buffer)
                          (ring-buffer-head ring-buffer) items))

(declaim (inline copy-to-ring-output-buffer))
(defun copy-to-ring-output-buffer (buf foreign-array items)
  (declare (type ring-output-buffer buf) (type foreign-pointer foreign-array)
           (type non-negative-fixnum items))
  (%copy-to-ring-output-buffer (ring-output-buffer-data buf)
                               foreign-array (ring-output-buffer-size buf)
                               (ring-output-buffer-head buf) items))

(declaim (inline ring-output-buffer-next))
(defun ring-output-buffer-next (buf)
  (declare (type ring-output-buffer buf))
  ;; Temporary variable on the foreign heap
  (setf (smp-ref (ring-output-buffer-tmp buf) 0)
        #1=(smp-ref (ring-output-buffer-data buf)
                    (ring-output-buffer-head buf)))
  (setf #1# +sample-zero+)
  (setf (ring-output-buffer-head buf)
        (logand (1+ (ring-output-buffer-head buf))
                (ring-output-buffer-mask buf)))
  (smp-ref (ring-output-buffer-tmp buf)))

(defstruct (analysis (:include incudine-object) (:copier nil))
  "Analysis type."
  (size 0 :type non-negative-fixnum)
  (input-buffer (null-pointer) :type foreign-pointer)
  (input-buffer-size 0 :type non-negative-fixnum)
  (input-changed-p nil :type boolean)
  (output-buffer (null-pointer) :type foreign-pointer)
  (output-buffer-size 0 :type non-negative-fixnum)
  (output-complex-p nil :type boolean)
  (nbins 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (time-ptr (null-pointer) :type foreign-pointer)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(setf
  (documentation 'analysis-p 'function)
  "Return T if object is of type ANALYSIS."
  (documentation 'analysis-input-buffer 'function)
  "Return the foreign pointer to the analysis input buffer."
  (documentation 'analysis-input-buffer-size 'function)
  "Return the analysis input buffer size."
  (documentation 'analysis-output-buffer 'function)
  "Return the foreign pointer to the analysis output buffer."
  (documentation 'analysis-output-buffer-size 'function)
  "Return the analysis output buffer size.")

(declaim (inline analysis-time))
(defun analysis-time (obj)
  "Return the time in samples of the last analysis. Setfable."
  (declare (type analysis obj))
  (smp-ref (analysis-time-ptr obj) 0))

(declaim (inline set-analysis-time))
(defun set-analysis-time (obj time)
  (declare (type analysis obj) (type sample time))
  (setf (smp-ref (analysis-time-ptr obj)) time))

(defsetf analysis-time set-analysis-time)

(defun touch-analysis (obj)
  "Update the modification time of the analysis."
  (declare (type analysis obj))
  (if (< (analysis-time obj) (now))
      (setf (analysis-time obj) (now)))
  (setf (analysis-input-changed-p obj) t)
  obj)

(declaim (inline discard-analysis))
(defun discard-analysis (obj)
  "Discard the analysis."
  (declare (type analysis obj))
  (setf (analysis-time obj) (1- (now)))
  (setf (analysis-input-changed-p obj) nil)
  obj)

#-(and sbcl x86 (not darwin))
(progn
  (define-constant +fftw-measure+ 0)
  (define-constant +fftw-patient+ (ash 1 5))
  (define-constant +fftw-estimate+ (ash 1 6)))

(define-constant +fft-plan-optimal+ +fftw-patient+
  :documentation "Slowest computation of an optimal FFT plan.")

(define-constant +fft-plan-best+ +fftw-measure+
  :documentation "Slow computation of an accurate FFT plan.")

(define-constant +fft-plan-fast+ +fftw-estimate+
  :documentation "Fast computation of a reasonable FFT plan.")

(defstruct (fft-plan (:constructor %new-fft-plan))
  "FFT planner type."
  (pair (list nil) :type cons)
  (size 8 :type positive-fixnum)
  (flags +fft-plan-best+ :type fixnum))

(defvar *dummy-fft-plan* (%new-fft-plan))
(declaim (type fft-plan *dummy-fft-plan*))

(defun rectangular-window (foreign-array size)
  "Fill a FOREIGN-ARRAY of the given SIZE and type SAMPLE with a
rectangular window."
  (declare (type foreign-pointer foreign-array) (type non-negative-fixnum size))
  (dotimes (i size foreign-array)
    (setf (smp-ref foreign-array i) #.(sample 1))))

(defstruct (fft-common (:include analysis) (:copier nil))
  (ring-buffer *dummy-ring-buffer* :type ring-buffer)
  (window-buffer (null-pointer) :type foreign-pointer)
  (window-size 0 :type non-negative-fixnum)
  (window-function #'rectangular-window :type (or function null))
  (plan-wrap *dummy-fft-plan* :type fft-plan)
  (plan (null-pointer) :type foreign-pointer))

(defstruct (fft (:include fft-common) (:constructor %make-fft) (:copier nil))
  "FFT type."
  (shift 0 :type fixnum))

(setf
  (documentation 'fft-p 'function)
  "Return T if object is of type FFT."
  (documentation 'fft-size 'function)
  "Return the FFT size."
  (documentation 'fft-plan 'function)
  "Return the planner of the FFT instance.")

(defstruct (ifft (:include fft-common) (:constructor %make-ifft) (:copier nil))
  "IFFT type.")

(setf
  (documentation 'ifft-p 'function)
  "Return T if object is of type IFFT."
  (documentation 'ifft-size 'function)
  "Return the IFFT size."
  (documentation 'ifft-plan 'function)
  "Return the planner of the IFFT instance.")

(define-constant +fft-pool-initial-size+ 200)

(defvar *fft-pool*
  (make-incudine-object-pool +fft-pool-initial-size+ #'%make-fft nil))
(declaim (type incudine-object-pool *fft-pool*))

(defvar *rt-fft-pool*
  (make-incudine-object-pool +fft-pool-initial-size+ #'%make-fft t))
(declaim (type incudine-object-pool *rt-fft-pool*))

(defvar *ifft-pool*
  (make-incudine-object-pool +fft-pool-initial-size+ #'%make-ifft nil))
(declaim (type incudine-object-pool *ifft-pool*))

(defvar *rt-ifft-pool*
  (make-incudine-object-pool +fft-pool-initial-size+ #'%make-ifft t))
(declaim (type incudine-object-pool *rt-ifft-pool*))

(defgeneric window-function (obj)
  (:documentation "NIL or the function of two arguments called to fill an analysis data window.
The function arguments are the foreign pointer to the data window of type
SAMPLE and the window size respectively. Setfable."))

(defgeneric (setf window-function) (fn obj))

(defgeneric window-size (obj)
  (:documentation "Return the analysis window size. Setfable."))

(defgeneric (setf window-size) (size obj))

(defmethod window-function ((obj fft-common))
  (fft-common-window-function obj))

(declaim (inline fill-window-buffer))
(defun fill-window-buffer (buffer function size &optional force-p)
  (if (and (not force-p) (eq function #'rectangular-window))
      buffer
      (funcall function buffer size)))

(defmethod (setf window-function) ((fn function) (obj fft-common))
  (fill-window-buffer (fft-common-window-buffer obj) fn
                      (fft-common-window-size obj))
  (setf (fft-common-window-function obj) fn))

(defmethod (setf window-function) ((fn null) (obj fft-common))
  (setf (fft-common-window-function obj) nil))

(defmethod window-size ((obj fft-common))
  (fft-common-window-size obj))

(defmethod (setf window-size) (size (obj fft-common))
  (declare (type positive-fixnum size))
  (when (> size (fft-size obj))
    (setf size (fft-size obj)))
  (unless (= size (fft-common-window-size obj))
    (setf (fft-common-window-size obj) size)
    (when (fft-common-window-function obj)
      (fill-window-buffer (fft-common-window-buffer obj)
                          (the function (fft-common-window-function obj))
                          size)))
  size)

(defstruct (abuffer (:include incudine-object)
                    (:constructor %make-abuffer)
                    (:copier nil))
  "Abuffer (Analysis Buffer) type."
  (data (null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (nbins 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (time-ptr (null-pointer) :type foreign-pointer)
  (link nil :type (or analysis null))
  (coord-complex-p nil :type boolean)
  (normalized-p nil :type boolean)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(setf
 (documentation 'abuffer-p 'function)
 "Return T if object is of type ABUFFER."
 (documentation 'abuffer-data 'function)
 "Return the foreign pointer to the abuffer data."
 (documentation 'abuffer-size 'function)
 "Return the abuffer data size."
 (documentation 'abuffer-nbins 'function)
 "Return the number of analysis bins of the abuffer."
 (documentation 'abuffer-link 'function)
 "Return the analysis object linked to the abuffer."
 (documentation 'abuffer-normalized-p 'function)
 "Whether the abuffer is normalized.")

(define-constant +abuffer-pool-initial-size+ 400)

(defvar *abuffer-pool*
  (make-incudine-object-pool +abuffer-pool-initial-size+ #'%make-abuffer nil))
(declaim (type incudine-object-pool *abuffer-pool*))

(defvar *rt-abuffer-pool*
  (make-incudine-object-pool +abuffer-pool-initial-size+ #'%make-abuffer t))
(declaim (type incudine-object-pool *rt-abuffer-pool*))

(defun make-abuffer (analysis-object
                     &optional (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new ABUFFER structure linked to ANALYSIS-OBJECT.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type analysis analysis-object))
  (flet ((foreign-alloc (size zero-p rt-p)
           (if rt-p
               (foreign-rt-alloc 'sample :count size :zero-p zero-p)
               (foreign-alloc-sample size))))
    (let* ((coord-complex-p (analysis-output-complex-p analysis-object))
           (%nbins (analysis-nbins analysis-object))
           (%size (if coord-complex-p
                      (* 2 %nbins)
                      (analysis-size analysis-object)))
           (rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
           (data-ptr nil)
           (tptr nil))
      (multiple-value-bind (obj free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-abuffer-pool*)
                      #'safe-foreign-rt-free
                      *rt-abuffer-pool*)
              (values (incudine.util::alloc-object *abuffer-pool*)
                      #'foreign-free
                      *abuffer-pool*))
        (declare (type abuffer obj) (type incudine-object-pool pool))
        (handler-case
            (progn
              (setf data-ptr (foreign-alloc %size t rt-p))
              (setf tptr (foreign-alloc 1 nil rt-p))
              (incudine.util::with-struct-slots
                  ((data size nbins scale-factor time-ptr link coord-complex-p
                    real-time-p foreign-free)
                   obj abuffer "INCUDINE.ANALYSIS")
                (setf data data-ptr
                      size %size
                      nbins %nbins
                      scale-factor (analysis-scale-factor analysis-object)
                      time-ptr tptr
                      link analysis-object
                      coord-complex-p coord-complex-p
                      real-time-p rt-p
                      foreign-free free-fn)
                (setf (smp-ref time-ptr 0) #.(sample -1))))
          (condition (c)
            (dolist (p (list data-ptr tptr))
              (if p (funcall free-fn p)))
            (incudine-object-pool-expand pool 1)
            (error c)))
        (incudine-finalize obj
          (lambda ()
            (funcall free-fn data-ptr)
            (funcall free-fn tptr)
            (incudine-object-pool-expand pool 1)))))))

(defmethod free-p ((obj abuffer))
  (zerop (abuffer-size obj)))

(defmethod free ((obj abuffer))
  (unless (free-p obj)
    (mapc (abuffer-foreign-free obj)
          (list (abuffer-data obj) (abuffer-time-ptr obj)))
    (setf (abuffer-size obj) 0)
    (incudine-cancel-finalization obj)
    (setf (abuffer-data obj) (null-pointer)
          (abuffer-time-ptr obj) (null-pointer)
          (abuffer-nbins obj) 0
          (abuffer-link obj) nil)
    (if (abuffer-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-abuffer-pool*)
        (incudine.util::free-object obj *abuffer-pool*))
    (incudine.util:nrt-msg debug "Free ~A" (type-of obj)))
  (values))

(defun abuffer-polar (obj)
  "Convert the representation of the abuffer data from complex to
polar if necessary."
  (when (abuffer-coord-complex-p obj)
    (complex-to-polar (abuffer-data obj) (abuffer-nbins obj))
    (setf (abuffer-coord-complex-p obj) nil)))

(defun abuffer-complex (obj)
  "Convert the representation of the abuffer data from polar to
complex if necessary."
  (unless (abuffer-coord-complex-p obj)
    (polar-to-complex (abuffer-data obj) (abuffer-nbins obj))
    (setf (abuffer-coord-complex-p obj) t)))

(declaim (inline abuffer-time))
(defun abuffer-time (obj)
  "Return the modification time in samples of the abuffer OBJ. Setfable."
  (smp-ref (abuffer-time-ptr obj) 0))

(declaim (inline set-abuffer-time))
(defun set-abuffer-time (obj time)
  (declare (type abuffer obj) (type sample time))
  (setf (smp-ref (abuffer-time-ptr obj)) time))

(defsetf abuffer-time set-abuffer-time)

(defun abuffer-realpart (obj nbin)
  "Return the real part of the analysis bin NBIN of the abuffer OBJ.
Setfable."
  (mem-ref (abuffer-data obj) 'sample
           (the non-negative-fixnum (* nbin +foreign-complex-size+))))

(define-compiler-macro abuffer-realpart (obj nbin)
  (if (constantp nbin)
      `(mem-ref (abuffer-data ,obj) 'sample
                ,(* (eval nbin) +foreign-complex-size+))
      `(mem-ref (abuffer-data ,obj) 'sample
                (the non-negative-fixnum (* ,nbin ,+foreign-complex-size+)))))

(defun set-abuffer-realpart (obj nbin value)
  (setf (mem-ref (abuffer-data obj) 'sample
                 (the non-negative-fixnum (* nbin +foreign-complex-size+)))
        (sample value)))

(define-compiler-macro set-abuffer-realpart (obj nbin value)
  (if (constantp nbin)
      `(setf (mem-ref (abuffer-data obj) 'sample
                      ,(* (eval nbin) +foreign-complex-size+))
             (sample ,value))
      `(setf (mem-ref (abuffer-data ,obj) 'sample
                      (the non-negative-fixnum
                        (* ,nbin ,+foreign-complex-size+)))
             (sample ,value))))

(defsetf abuffer-realpart set-abuffer-realpart)

(defun abuffer-imagpart (obj nbin)
  "Return the imaginary part of the analysis bin NBIN of the abuffer OBJ.
Setfable."
  (mem-ref (abuffer-data obj) 'sample
           (the non-negative-fixnum
             (+ (the non-negative-fixnum (* nbin +foreign-complex-size+))
                +foreign-sample-size+))))

(define-compiler-macro abuffer-imagpart (obj nbin)
  (if (constantp nbin)
      `(mem-ref (abuffer-data ,obj) 'sample
                ,(+ (* (eval nbin) +foreign-complex-size+)
                    +foreign-complex-size+))
      `(mem-ref (abuffer-data ,obj) 'sample
                (the non-negative-fixnum
                  (+ (the non-negative-fixnum
                       (* ,nbin ,+foreign-complex-size+))
                     ,+foreign-sample-size+)))))

(defun set-abuffer-imagpart (obj nbin value)
  (setf (mem-ref (abuffer-data obj) 'sample
                 (the non-negative-fixnum
                   (+ (the non-negative-fixnum
                        (* nbin +foreign-complex-size+))
                      +foreign-sample-size+)))
        (sample value)))

(define-compiler-macro set-abuffer-imagpart (obj nbin value)
  (if (constantp nbin)
      `(setf (mem-ref (abuffer-data ,obj) 'sample
                      (+ ,(* (eval nbin) +foreign-complex-size+)
                         ,+foreign-sample-size+))
             (sample ,value))
      `(setf (mem-ref (abuffer-data ,obj) 'sample
                      (the non-negative-fixnum
                        (+ (the non-negative-fixnum
                             (* ,nbin ,+foreign-complex-size+))
                           ,+foreign-sample-size+)))
             (sample ,value))))

(defsetf abuffer-imagpart set-abuffer-imagpart)

(defgeneric update-linked-object (obj force-p)
  (:documentation "Called within COMPUTE-ABUFFER to update the linked object."))

(defmethod update-linked-object ((obj t) force-p)
  (declare (ignore obj force-p))
  nil)

(defgeneric update-linked-object-p (obj))

(defmethod update-linked-object-p ((obj t))
  (declare (ignore obj))
  nil)

(defun compute-abuffer (abuf &optional force-p)
  "Fill the abuffer with the updated analysis data of the linked
analysis object.

If FORCE-P is NIL (default), the abuffer is updated once for the
current time."
  (declare (type abuffer abuf) (type boolean force-p))
  (let ((link (abuffer-link abuf)))
    (when (or force-p
              (< (abuffer-time abuf) (now))
              (update-linked-object-p link))
      (update-linked-object link force-p)
      (setf (abuffer-coord-complex-p abuf) (analysis-output-complex-p link))
      (setf (abuffer-time abuf) (now))
      (foreign-copy-samples (abuffer-data abuf) (analysis-output-buffer link)
                            (abuffer-size abuf))))
  abuf)

(declaim (inline touch-abuffer))
(defun touch-abuffer (obj)
  "Update the modification time of the abuffer data."
  (declare (type abuffer obj))
  (when (< (abuffer-time obj) (now))
    (setf (abuffer-time obj) (now)))
  obj)

(declaim (inline discard-abuffer))
(defun discard-abuffer (obj)
  "Discard the retrieved data of the ABUFFER object OBJ."
  (declare (type abuffer obj))
  (setf (abuffer-time obj) (1- (now)))
  obj)

(defmacro dofft ((index-var nbins-var abuffer-src-list abuffer-dest-list
                  x-var-prefix y-var-prefix &key coord-complex-p
                  (index-start 0) index-end (coord-check-p t) init result)
                 &body body)
  "Iterate over the complex values of one or more ABUFFER structures
in ABUFFER-SRC-LIST and ABUFFER-DEST-LIST with INDEX-VAR bound to each
number of analysis bin from INDEX-START to INDEX-END, then RESULT form
is evaluated.

NBINS-VAR is bound to the number of analysis bins that it is supposed
to be the same for all the abuffer's.

INDEX-START and INDEX-END default to 0 and the number of analysis bins.

The strings X-VAR-PREFIX and Y-VAR-PREFIX are the prefixes of the real
and imaginary parts of a complex number, respectively. For example, if
X-VAR-PREFIX is \"MAG\", the variables MAG0, MAG1 and MAG2 are bound
to the real parts of each complex value related to the first three
abuffer's.

If COORD-CHECK-P is T (default), the representation of the data for
the abuffer's in ABUFFER-SRC-LIST is converted if necessary.

The optional INIT form is inserted before the loop.

The modification time of the abuffer's in ABUFFER-DEST-LIST is updated."
  (with-gensyms (start end)
    (let* ((abuffer-src-vars (loop for i below (length abuffer-src-list)
                                   collect (gensym "ABUF")))
           (abuffer-dest-vars (loop for i below (length abuffer-dest-list)
                                    collect (gensym "ABUF")))
           (abuffer-full-vars (append abuffer-src-vars abuffer-dest-vars))
           (abuf0 (car (or abuffer-src-vars abuffer-dest-vars))))
      `(let (,@(mapcar (lambda (var abuf) `(,var ,abuf))
                       abuffer-src-vars abuffer-src-list)
             ,@(mapcar (lambda (var abuf) `(,var ,abuf))
                       abuffer-dest-vars abuffer-dest-list))
         (let ((,nbins-var (abuffer-nbins ,abuf0)))
           (declare (type non-negative-fixnum ,nbins-var)
                    (ignorable ,nbins-var))
           ,@(when coord-check-p
               `(,(if coord-complex-p
                      `(progn
                         ,@(mapcar (lambda (x)
                                     `(unless (abuffer-coord-complex-p ,x)
                                        (polar-to-complex (abuffer-data ,x)
                                                          ,nbins-var)
                                        (setf (abuffer-coord-complex-p ,x) t)))
                                   abuffer-src-vars)
                         ,@(mapcar (lambda (x)
                                     `(unless (abuffer-coord-complex-p ,x)
                                        (setf (abuffer-coord-complex-p ,x) t)))
                                   abuffer-dest-vars))
                      `(progn
                         ,@(mapcar (lambda (x)
                                     `(when (abuffer-coord-complex-p ,x)
                                        (complex-to-polar (abuffer-data ,x)
                                                          ,nbins-var)
                                        (setf (abuffer-coord-complex-p ,x) nil)))
                                   abuffer-src-vars)
                         ,@(mapcar (lambda (x)
                                     `(when (abuffer-coord-complex-p ,x)
                                        (setf (abuffer-coord-complex-p ,x) nil)))
                                   abuffer-dest-vars)))))
           (let ((,start ,index-start)
                 (,end ,(or index-end nbins-var)))
             (declare (type non-negative-fixnum ,start ,end))
             ,init
             (do ((,index-var ,start (1+ ,index-var)))
                 ((>= ,index-var ,end))
               (declare (type non-negative-fixnum ,index-var))
               (symbol-macrolet
                   ,(loop for abuf in abuffer-full-vars
                          for count from 0
                          collect `(,(format-symbol *package* "~A~D"
                                                    x-var-prefix count)
                                    (abuffer-realpart ,abuf ,index-var))
                          collect `(,(format-symbol *package* "~A~D"
                                                    y-var-prefix count)
                                    (abuffer-imagpart ,abuf ,index-var)))
                 ,@body)))
           ;; Update the time of the destinations
           ,@(mapcar (lambda (abuf) `(touch-abuffer ,abuf)) abuffer-dest-vars)
           ,result)))))

(defmacro dofft-polar ((index-var nbins-var abuffer-src-list abuffer-dest-list
                        &key (index-start 0) index-end (coord-check-p t)
                        init result) &body body)
  "Iterate over the complex values of one or more ABUFFER structures
in ABUFFER-SRC-LIST and ABUFFER-DEST-LIST by using polar coordinates,
with INDEX-VAR bound to each number of analysis bin from INDEX-START
to INDEX-END, then RESULT form is evaluated.

NBINS-VAR is bound to the number of analysis bins that it is supposed
to be the same for all the abuffer's.

INDEX-START and INDEX-END default to 0 and the number of analysis bins.

\"MAG\" and \"PHASE\" are the prefixes of the variables bound to the
real and imaginary parts of each complex value of the abuffer's: MAG0
and PHASE0 for the first abuffer, MAG1 and PHASE1 for the second, etc.

If COORD-CHECK-P is T (default), the representation of the data for
the abuffer's in ABUFFER-SRC-LIST is converted if necessary.

The optional INIT form is inserted before the loop.

The modification time of the abuffer's in ABUFFER-DEST-LIST is updated.

Examples:

    (dofft-polar (i nbins ((compute-abuffer abuf)) () :result abuf)
      (when (>= i threshold)
        (setf mag0 +sample-zero+)))

    (dofft-polar (i nbins ((compute-abuffer abuf-src)) (abuf-dest)
                  :result abuf-dest :index-start 1 :index-end (1- nbins))
      (if (or (< mag0 threshold)
              (< (abuffer-realpart abuf-src (1- i)) threshold)
              (< (abuffer-realpart abuf-src (1+ i)) threshold))
          (setf mag1 +sample-zero+ phase1 +sample-zero+)
          (setf mag1 mag0 phase1 phase0)))"
  `(dofft (,index-var ,nbins-var ,abuffer-src-list ,abuffer-dest-list
           "MAG" "PHASE" :coord-complex-p nil :index-start ,index-start
           :index-end ,index-end :coord-check-p ,coord-check-p :init ,init
           :result ,result)
     ,@body))

(defmacro dofft-complex ((index-var nbins-var abuffer-src-list abuffer-dest-list
                          &key (index-start 0) index-end (coord-check-p t)
                          init result) &body body)
  "Iterate over the complex values of one or more ABUFFER structures
in ABUFFER-SRC-LIST and ABUFFER-DEST-LIST by using complex coordinates,
with INDEX-VAR bound to each number of analysis bin from INDEX-START
to INDEX-END, then RESULT form is evaluated.

NBINS-VAR is bound to the number of analysis bins that it is supposed
to be the same for all the abuffer's.

INDEX-START and INDEX-END default to 0 and the number of analysis bins.

\"RE\" and \"IM\" are the prefixes of the variables bound to the real
and imaginary parts of each complex value of the abuffer's: RE0 and
IM0 for the first abuffer, RE1 and IM1 for the second, etc.

If COORD-CHECK-P is T (default), the representation of the data for
the abuffer's in ABUFFER-SRC-LIST is converted if necessary.

The optional INIT form is inserted before the loop.

The modification time of the abuffer's in ABUFFER-DEST-LIST is updated."
  `(dofft (,index-var ,nbins-var ,abuffer-src-list ,abuffer-dest-list "RE" "IM"
           :coord-complex-p t :index-start ,index-start :index-end ,index-end
           :coord-check-p ,coord-check-p :init ,init :result ,result)
     ,@body))
