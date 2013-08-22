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

(in-package :incudine.analysis)

(defvar *fft-default-window-function* (incudine.gen:sine-window))
(declaim (type function *fft-default-window-function*))

(defstruct (fft-common (:include analysis)
                       (:copier nil))
  (size 0 :type non-negative-fixnum)
  (nbins 0 :type non-negative-fixnum)
  (ring-buffer (error "missing RING-BUFFER") :type ring-buffer)
  (window-buffer (error "missing WINDOW-BUFFER") :type foreign-pointer)
  (window-size 0 :type non-negative-fixnum)
  (window-function (error "missing WINDOW-FUNCTION") :type function)
  (plan (null-pointer) :type foreign-pointer))

(defmethod free ((obj fft-common))
  (unless (= (fft-common-size obj) 0)
    (mapc (analysis-foreign-free obj)
          (list #1=(analysis-input-buffer obj)
                #2=(analysis-output-buffer obj)
                #3=(analysis-time-ptr obj)))
    (setf #1# (null-pointer) #2# (null-pointer) #3# (null-pointer))
    (free (fft-common-ring-buffer obj))
    (fft-destroy-plan (fft-common-plan obj))
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

(defgeneric nbins (obj))

(defmethod nbins ((obj fft))
  (fft-nbins obj))

(defmethod nbins ((obj abuffer))
  (if (fft-p #1=(abuffer-link obj))
      (fft-nbins #1#)
      0))

(define-constant +fftw-measure+ 0)
(define-constant +fftw-estimate+ (ash 1 6))

(declaim (inline rectangular-window))
(defun rectangular-window (c-array size)
  (declare (type foreign-pointer c-array)
           (type non-negative-fixnum size))
  (dotimes (i size c-array)
    (setf (data-ref c-array i) #.(sample 1))))

(declaim (inline fill-window-buffer))
(defun fill-window-buffer (buffer function size)
  (if (eq function #'rectangular-window)
      ;; Skip to fill the buffer
      buffer
      (funcall function buffer size)))

(defun make-fft (size &key (window-size 0)
                 (window-function *fft-default-window-function*)
                 (flags +fftw-estimate+) real-time-p)
  (declare (type non-negative-fixnum size window-size flags)
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
                 :time-ptr time-ptr
                 :real-time-p real-time-p
                 :foreign-free (if real-time-p
                                   #'foreign-rt-free
                                   #'foreign-free)
                 :plan (make-fft-plan size input-buffer output-buffer flags))))
      (setf (analysis-time obj) (sample -1))
      (foreign-zero-sample input-buffer size)
      (let ((plan (fft-plan obj))
            (foreign-free (fft-foreign-free obj)))
        (tg:finalize obj (lambda ()
                           (rt-eval-if (real-time-p)
                             (mapc foreign-free
                                   (list input-buffer output-buffer
                                         window-buffer time-ptr))
                             (fft-destroy-plan plan))))
        obj))))

(defmethod compute ((obj fft) &optional arg)
  (declare (ignore arg))
  (when (< (analysis-time obj) (now))
    (copy-from-ring-buffer (fft-input-buffer obj)
                           (fft-ring-buffer obj)
                           (fft-size obj))
    (unless (eq (fft-window-function obj) #'rectangular-window)
      (apply-window (fft-input-buffer obj)
                    (fft-window-buffer obj)
                    (fft-window-size obj)))
    (apply-zero-padding (fft-input-buffer obj)
                        (fft-window-size obj)
                        (fft-size obj))
    (fftw-execute (fft-plan obj))
    (setf (analysis-time obj) (now))
    t))

(declaim (inline fft-input))
(defun fft-input (fft)
  (let ((buf (fft-ring-buffer fft)))
    (mem-ref (ring-input-buffer-data buf) 'sample
             (the non-negative-fixnum
               (* (ring-input-buffer-head buf)
                  +foreign-sample-size+)))))

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
                  (flags +fftw-estimate+) real-time-p)
  (declare (type non-negative-fixnum size window-size flags)
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
                                   #'foreign-rt-free
                                   #'foreign-free)
                 :plan (make-ifft-plan size input-buffer output-buffer flags))))
      (setf (analysis-time obj) (sample -1))
      (foreign-zero-sample input-buffer complex-array-size)
      (let ((plan (ifft-plan obj))
            (foreign-free (ifft-foreign-free obj)))
        (tg:finalize obj (lambda ()
                           (rt-eval-if (real-time-p)
                             (mapc foreign-free
                                   (list input-buffer output-buffer
                                         window-buffer time-ptr))
                             (fft-destroy-plan plan))))
        obj))))

(declaim (inline ifft-apply-window))
(defun ifft-apply-window (obj abuf)
  (declare (type ifft obj) (type abuffer abuf))
  (cond ((abuffer-normalized-p abuf)
         (unless (eq (ifft-window-function obj) #'rectangular-window)
           (apply-window (ifft-output-buffer obj) (ifft-window-buffer obj)
                         (ifft-window-size obj))))
        ((eq (ifft-window-function obj) #'rectangular-window)
         (apply-scaled-rectwin (ifft-output-buffer obj) (ifft-window-size obj)
                               (ifft-size obj)))
        (t (apply-scaled-window (ifft-output-buffer obj) (ifft-window-buffer obj)
                                (ifft-window-size obj) (ifft-size obj)))))

(defmethod compute ((obj ifft) &optional arg)
  (when (< (analysis-time obj) (now))
    (compute arg)
    (unless (abuffer-coord-complex-p arg)
      (polar-to-complex (abuffer-data arg)
                        (abuffer-nbins arg)))
    (foreign-copy (ifft-input-buffer obj) (abuffer-data arg)
                  (the non-negative-fixnum
                    (* (the non-negative-fixnum (ifft-input-size obj))
                       +foreign-sample-size+)))
    (fftw-execute (ifft-plan obj))
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
