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

(in-package :incudine.analysis)

(defvar *fft-default-window-function* (incudine.gen:sine-window))
(declaim (type function *fft-default-window-function*))

(defvar *fft-plan* (make-hash-table :size 16))
(declaim (type hash-table *fft-plan*))

(define-constant +fftw-measure+ 0)
(define-constant +fftw-patient+ (ash 1 5))
(define-constant +fftw-estimate+ (ash 1 6))

(define-constant +fft-plan-optimal+ +fftw-patient+
  :documentation "Slowest computation of an optimal FFT plan.")

(define-constant +fft-plan-best+ +fftw-measure+
  :documentation "Slow computation of an accurate FFT plan.")

(define-constant +fft-plan-fast+ +fftw-estimate+
  :documentation "Fast computation of a reasonable FFT plan.")

(declaim (inline get-fft-plan))
(defun get-fft-plan (size)
  (values (gethash size *fft-plan*)))

(declaim (inline add-fft-plan))
(defun add-fft-plan (size pair)
  (setf (gethash size *fft-plan*) pair))

(declaim (inline remove-fft-plan))
(defun remove-fft-plan (size)
  (remhash size *fft-plan*))

(defstruct (fft-plan (:constructor %new-fft-plan))
  (pair (error "missing FFT plans") :type cons)
  (size 8 :type positive-fixnum)
  (flags +fft-plan-best+ :type fixnum))

(defmethod print-object ((obj fft-plan) stream)
  (format stream "#<FFT-PLAN :SIZE ~D :FLAGS ~D>"
          (fft-plan-size obj) (fft-plan-flags obj)))

(defun fft-plan-list (&optional only-size-p)
  "Return the list of the stored FFT-PLANs. If ONLY-SIZE-P is T,
the values of the list are the sizes of the plans."
  (if only-size-p
      (sort (loop for size being the hash-keys in *fft-plan*
                  collect size)
            #'<)
      (sort (loop for plan being the hash-values in *fft-plan*
                  collect plan)
            #'< :key #'fft-plan-size)))

(defun compute-fft-plan (size flags realtime-p)
  "Return a CONS where the CAR is the plan for a FFT and the CDR
is the plan for a IFFT."
  (declare (type positive-fixnum size) (type fixnum flags)
           (type boolean realtime-p))
  (flet ((alloc-buffer (size)
           (if realtime-p
               (foreign-rt-alloc 'sample :count size)
               (foreign-alloc-fft size)))
         (free-buffer (ptr)
           (if realtime-p
               (foreign-rt-free ptr)
               (foreign-free ptr))))
    (let* ((maxsize (* (1+ (ash size -1)) 2))
           (inbuf (alloc-buffer maxsize))
           (outbuf (alloc-buffer maxsize)))
      (declare (type positive-fixnum maxsize))
      (unwind-protect
           (cons (make-fft-plan size inbuf outbuf flags)
                 (make-ifft-plan size inbuf outbuf flags))
        (free-buffer inbuf)
        (free-buffer outbuf)))))

(declaim (inline %%new-fft-plan))
(defun %%new-fft-plan (size flags realtime-p)
  (let ((pair (compute-fft-plan size flags realtime-p)))
    (tg:finalize (add-fft-plan size (%new-fft-plan :pair pair :size size
                                                   :flags flags))
                 (lambda ()
                   (fft-destroy-plan (car pair))
                   (fft-destroy-plan (cdr pair))))))

(defun new-fft-plan (size &optional flags)
  "Calculate and store a new FFT-PLAN with the specified size."
  (declare (type positive-fixnum size)
           (type (or fixnum null) flags)
           #.*standard-optimize-settings*)
  (let ((plan (get-fft-plan size))
        (realtime-p (rt-thread-p)))
    (if plan
        (if realtime-p
            plan
            (let ((flags (or flags +fft-plan-best+)))
              (if (or (= (fft-plan-flags plan) flags)
                      (= (fft-plan-flags plan) +fft-plan-optimal+))
                  plan
                  ;; Re-compute the plan
                  (%%new-fft-plan size flags nil))))
        (%%new-fft-plan size (if realtime-p
                                 +fft-plan-fast+
                                 (or flags +fft-plan-best+))
                        realtime-p))))

(defstruct (fft-common (:include analysis)
                       (:copier nil))
  (size 0 :type non-negative-fixnum)
  (nbins 0 :type non-negative-fixnum)
  (ring-buffer (error "missing RING-BUFFER") :type ring-buffer)
  (window-buffer (error "missing WINDOW-BUFFER") :type foreign-pointer)
  (window-size 0 :type non-negative-fixnum)
  (window-function (error "missing WINDOW-FUNCTION") :type function)
  (plan-wrap (error "missing FFT plan wrapper") :type fft-plan)
  (plan (error "missing FFT plan") :type foreign-pointer))

(defmethod free ((obj fft-common))
  (unless (= (fft-common-size obj) 0)
    (mapc (analysis-foreign-free obj)
          (list #1=(analysis-input-buffer obj)
                #2=(analysis-output-buffer obj)
                #3=(analysis-time-ptr obj)))
    (setf #1# (null-pointer) #2# (null-pointer) #3# (null-pointer))
    (free (fft-common-ring-buffer obj))
    (setf (fft-common-size obj) 0))
  (unless (= (fft-common-window-size obj) 0)
    (funcall (analysis-foreign-free obj)
             (fft-common-window-buffer obj))
    (setf (fft-common-window-buffer obj) (null-pointer))
    (setf (fft-common-window-size obj) 0))
  (tg:cancel-finalization obj)
  (values))

(defstruct (fft (:include fft-common)
                (:constructor %make-fft)
                (:copier nil))
  (output-size 0 :type non-negative-fixnum))

(defmethod print-object ((obj fft) stream)
    (format stream "#<FFT :SIZE ~D :WINDOW-SIZE ~D :NBINS ~D>"
            (fft-size obj) (fft-window-size obj) (fft-nbins obj)))

(declaim (inline rectangular-window))
(defun rectangular-window (c-array size)
  (declare (type foreign-pointer c-array)
           (type non-negative-fixnum size))
  (dotimes (i size c-array)
    (setf (smp-ref c-array i) #.(sample 1))))

(declaim (inline fill-window-buffer))
(defun fill-window-buffer (buffer function size)
  (if (eq function #'rectangular-window)
      ;; Skip to fill the buffer
      buffer
      (funcall function buffer size)))

(defun make-fft (size &key (window-size 0)
                 (window-function *fft-default-window-function*)
                 flags real-time-p)
  (declare (type non-negative-fixnum size window-size)
           (type (or fixnum null) flags)
           (type function window-function))
  (flet ((foreign-alloc (size fftw-array-p)
           (cond (real-time-p
                  (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p
                  (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size)
              (> window-size size))
      (setf window-size size))
    (let* ((nbins (1+ (ash size -1)))
           (complex-array-size (* 2 nbins))
           (input-buffer (foreign-alloc size t))
           (output-buffer (foreign-alloc complex-array-size t))
           (window-buffer (fill-window-buffer (foreign-alloc window-size nil)
                                              window-function window-size))
           (time-ptr (foreign-alloc 1 nil))
           (plan-wrap (new-fft-plan size flags))
           (obj (%make-fft
                 :size size
                 :input-buffer input-buffer
                 :output-buffer output-buffer
                 :output-size complex-array-size
                 :ring-buffer (make-ring-input-buffer size real-time-p)
                 :window-buffer window-buffer
                 :window-size window-size
                 :window-function window-function
                 :nbins nbins
                 :output-complex-p t
                 :scale-factor (/ (sample 1) size)
                 :time-ptr time-ptr
                 :real-time-p real-time-p
                 :foreign-free (if real-time-p
                                   #'safe-foreign-rt-free
                                   #'foreign-free)
                 :plan-wrap plan-wrap
                 :plan (car (fft-plan-pair plan-wrap)))))
      (setf (analysis-time obj) (sample -1))
      (foreign-zero-sample input-buffer size)
      (let ((foreign-free (fft-foreign-free obj)))
        (tg:finalize obj (lambda ()
                           (mapc foreign-free
                                 (list input-buffer output-buffer
                                       window-buffer time-ptr))))))))

(defun compute-fft (obj)
  (declare (type fft obj))
  (when (< (analysis-time obj) (now))
    (let ((fftsize (fft-size obj))
          (winsize (fft-window-size obj)))
    (copy-from-ring-buffer (fft-input-buffer obj)
                           (fft-ring-buffer obj)
                           fftsize)
    (unless (eq (fft-window-function obj) #'rectangular-window)
      (apply-window (fft-input-buffer obj)
                    (fft-window-buffer obj)
                    winsize))
    (apply-zero-padding (fft-input-buffer obj) winsize fftsize)
    (fft-execute (fft-plan obj)
                 (fft-input-buffer obj)
                 (fft-output-buffer obj))
    (setf (analysis-time obj) (now))
    t)))

(declaim (inline fft-input))
(defun fft-input (fft)
  (let ((buf (fft-ring-buffer fft)))
    (smp-ref (ring-input-buffer-data buf)
             (ring-input-buffer-head buf))))

(declaim (inline set-fft-input))
(defun set-fft-input (fft input)
  (ring-input-buffer-put input (fft-ring-buffer fft)))

(defsetf fft-input set-fft-input)

(defstruct (ifft (:include fft-common)
                 (:constructor %make-ifft)
                 (:copier nil))
  (input-size 0 :type non-negative-fixnum))

(defmethod print-object ((obj ifft) stream)
  (format stream "#<IFFT :SIZE ~D :WINDOW-SIZE ~D :NBINS ~D>"
          (ifft-size obj) (ifft-window-size obj) (ifft-nbins obj)))

(defun make-ifft (size &key (window-size 0)
                  (window-function *fft-default-window-function*)
                  flags real-time-p)
  (declare (type non-negative-fixnum size window-size)
           (type (or fixnum null) flags)
           (type function window-function))
  (flet ((foreign-alloc (size fftw-array-p)
           (cond (real-time-p
                  (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p
                  (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size)
              (> window-size size))
      (setf window-size size))
    (let* ((nbins (1+ (ash size -1)))
           (complex-array-size (* 2 nbins))
           (input-buffer (foreign-alloc complex-array-size t))
           (output-buffer (foreign-alloc size t))
           (window-buffer (fill-window-buffer (foreign-alloc window-size nil)
                                              window-function window-size))
           (time-ptr (foreign-alloc 1 nil))
           (plan-wrap (new-fft-plan size flags))
           (obj (%make-ifft
                 :size size
                 :input-buffer input-buffer
                 :input-size complex-array-size
                 :output-buffer output-buffer
                 :ring-buffer (make-ring-output-buffer size real-time-p)
                 :window-buffer window-buffer
                 :window-size window-size
                 :window-function window-function
                 :nbins nbins
                 :time-ptr time-ptr
                 :real-time-p real-time-p
                 :foreign-free (if real-time-p
                                   #'safe-foreign-rt-free
                                   #'foreign-free)
                 :plan-wrap plan-wrap
                 :plan (cdr (fft-plan-pair plan-wrap)))))
      (setf (analysis-time obj) (sample -1))
      (foreign-zero-sample input-buffer complex-array-size)
      (let ((foreign-free (ifft-foreign-free obj)))
        (tg:finalize obj (lambda ()
                           (mapc foreign-free
                                 (list input-buffer output-buffer
                                       window-buffer time-ptr))))))))

(declaim (inline ifft-apply-window))
(defun ifft-apply-window (obj abuf)
  (declare (type ifft obj) (type abuffer abuf))
  (cond ((abuffer-normalized-p abuf)
         (unless (eq (ifft-window-function obj) #'rectangular-window)
           (apply-window (ifft-output-buffer obj) (ifft-window-buffer obj)
                         (ifft-window-size obj))))
        ((eq (ifft-window-function obj) #'rectangular-window)
         (apply-scaled-rectwin (ifft-output-buffer obj) (ifft-window-size obj)
                               (abuffer-scale-factor abuf)))
        (t (apply-scaled-window (ifft-output-buffer obj) (ifft-window-buffer obj)
                                (ifft-window-size obj) (abuffer-scale-factor abuf)))))

(defun compute-ifft (obj arg)
  (declare (type ifft obj) (type (or fft abuffer) arg))
  (when (< (analysis-time obj) (now))
    (if (fft-p arg) (compute-fft arg) (compute-abuffer arg))
    (unless (abuffer-coord-complex-p arg)
      (polar-to-complex (abuffer-data arg)
                        (abuffer-nbins arg)))
    (foreign-copy (ifft-input-buffer obj) (abuffer-data arg)
                  (the non-negative-fixnum
                    (* (the non-negative-fixnum (ifft-input-size obj))
                       +foreign-sample-size+)))
    (ifft-execute (ifft-plan obj)
                  (ifft-input-buffer obj)
                  (ifft-output-buffer obj))
    (ifft-apply-window obj arg)
    (apply-zero-padding (ifft-output-buffer obj) (ifft-window-size obj)
                        (ifft-size obj))
    (copy-to-ring-output-buffer (ifft-ring-buffer obj)
                                (ifft-output-buffer obj)
                                (ifft-size obj))
    (setf (analysis-time obj) (now))
    t))

(declaim (inline ifft-output))
(defun ifft-output (ifft)
  (ring-output-buffer-next (ifft-ring-buffer ifft)))
