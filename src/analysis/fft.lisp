;;; Copyright (c) 2013-2017 Tito Latini
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

(declaim (inline get-fft-plan))
(defun get-fft-plan (size)
  (values (gethash size *fft-plan*)))

(declaim (inline add-fft-plan))
(defun add-fft-plan (size pair)
  (setf (gethash size *fft-plan*) pair))

(declaim (inline remove-fft-plan))
(defun remove-fft-plan (size)
  (remhash size *fft-plan*))

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
  (declare (type positive-fixnum size) (type (or fixnum null) flags)
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
                                 (or flags +fft-plan-fast+))
                        realtime-p))))

(defmethod free-p ((obj fft-common))
  (zerop (fft-common-size obj)))

(defmethod free ((obj fft-common))
  (unless (free-p obj)
    (mapc (analysis-foreign-free obj)
          (list #1=(analysis-input-buffer obj)
                #2=(analysis-output-buffer obj)
                #3=(analysis-time-ptr obj)))
    (setf #1# (null-pointer) #2# (null-pointer) #3# (null-pointer))
    (free (fft-common-ring-buffer obj))
    (setf (fft-common-size obj) 0))
  (unless (= (fft-common-window-size obj) 0)
    (funcall (analysis-foreign-free obj) (fft-common-window-buffer obj))
    (setf (fft-common-window-buffer obj) (null-pointer))
    (setf (fft-common-window-size obj) 0))
  (incudine-cancel-finalization obj)
  (incudine.util:nrt-msg debug "Free ~A" (type-of obj))
  (values))

(defmethod print-object ((obj fft) stream)
    (format stream "#<FFT :SIZE ~D :WINDOW-SIZE ~D :NBINS ~D>"
            (fft-size obj) (fft-window-size obj) (fft-nbins obj)))

(declaim (inline rectangular-window))
(defun rectangular-window (c-array size)
  (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
  (dotimes (i size c-array)
    (setf (smp-ref c-array i) #.(sample 1))))

(defun make-fft (size &key (window-size 0)
                 (window-function *fft-default-window-function*)
                 flags real-time-p)
  (declare (type non-negative-fixnum size window-size)
           (type (or fixnum null) flags) (type function window-function))
  (flet ((foreign-alloc (size fftw-array-p)
           (cond (real-time-p (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size) (> window-size size))
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
                 :input-size size
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
        (incudine-finalize obj (lambda ()
                                 (mapc foreign-free
                                       (list input-buffer output-buffer
                                             window-buffer time-ptr))))))))

(defun compute-fft (obj &optional force-p)
  (declare (type fft obj) (type boolean force-p))
  (when (or force-p (fft-input-changed-p obj))
    (let ((fftsize (fft-size obj))
          (winsize (fft-window-size obj)))
      (copy-from-ring-buffer
        (fft-input-buffer obj) (fft-ring-buffer obj) fftsize)
      (unless (eq (fft-window-function obj) #'rectangular-window)
        (apply-window (fft-input-buffer obj) (fft-window-buffer obj) winsize))
      (apply-zero-padding (fft-input-buffer obj) winsize fftsize)
      (fft-execute (fft-plan obj) (fft-input-buffer obj) (fft-output-buffer obj))
      (setf (analysis-time obj) (now))
      (setf (fft-input-changed-p obj) nil)))
  obj)

(defmethod update-linked-object ((obj fft) force-p)
  (compute-fft obj force-p))

(declaim (inline fft-input))
(defun fft-input (fft)
  (let ((buf (fft-ring-buffer fft)))
    (smp-ref (ring-input-buffer-data buf)
             (ring-input-buffer-head buf))))

(declaim (inline set-fft-input))
(defun set-fft-input (fft input)
  (setf (fft-input-changed-p fft) t)
  (ring-input-buffer-put input (fft-ring-buffer fft)))

(defsetf fft-input set-fft-input)

(defmethod print-object ((obj ifft) stream)
  (format stream "#<IFFT :SIZE ~D :WINDOW-SIZE ~D :NBINS ~D>"
          (ifft-size obj) (ifft-window-size obj) (ifft-nbins obj)))

(defun make-ifft (size &key (window-size 0)
                  (window-function *fft-default-window-function*)
                  flags real-time-p)
  (declare (type non-negative-fixnum size window-size)
           (type (or fixnum null) flags) (type function window-function))
  (flet ((foreign-alloc (size fftw-array-p)
           (cond (real-time-p (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size) (> window-size size))
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
                 :output-size size
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
        (incudine-finalize obj (lambda ()
                                 (mapc foreign-free
                                       (list input-buffer output-buffer
                                             window-buffer time-ptr))))))))

(declaim (inline ifft-apply-window))
(defun ifft-apply-window (obj &optional abuf)
  (declare (type ifft obj) (type (or abuffer null) abuf))
  (let ((outbuf (ifft-output-buffer obj))
        (winbuf (ifft-window-buffer obj))
        (winsize (ifft-window-size obj)))
    (cond ((or (null abuf) (abuffer-normalized-p abuf))
           (unless (eq (ifft-window-function obj) #'rectangular-window)
             (apply-window outbuf winbuf winsize)))
          ((eq (ifft-window-function obj) #'rectangular-window)
           (apply-scaled-rectwin outbuf winsize (abuffer-scale-factor abuf)))
          (t (apply-scaled-window outbuf winbuf winsize
                                  (abuffer-scale-factor abuf))))))

(defun compute-ifft (obj &optional abuffer force-p)
  (declare (type ifft obj) (type (or abuffer null) abuffer)
           (type boolean force-p))
  (when (or force-p
            (and abuffer (< (analysis-time obj) (now)))
            (ifft-input-changed-p obj))
    (when abuffer
      (compute-abuffer abuffer)
      (abuffer-complex abuffer)
      (foreign-copy-samples (ifft-input-buffer obj) (abuffer-data abuffer)
                            (ifft-input-size obj)))
    (ifft-execute (ifft-plan obj) (ifft-input-buffer obj)
                  (ifft-output-buffer obj))
    (ifft-apply-window obj abuffer)
    (apply-zero-padding (ifft-output-buffer obj) (ifft-window-size obj)
                        (ifft-size obj))
    (copy-to-ring-output-buffer (ifft-ring-buffer obj) (ifft-output-buffer obj)
                                (ifft-size obj))
    (setf (analysis-time obj) (now))
    (setf (ifft-input-changed-p obj) nil))
  obj)

(declaim (inline ifft-output))
(defun ifft-output (ifft)
  (ring-output-buffer-next (ifft-ring-buffer ifft)))

(defmethod circular-shift ((obj ifft) n)
  ;; Shifting the ring buffer head is also faster but GEN:ANALYSIS
  ;; does a copy of the output buffer.
  (incudine::foreign-circular-shift (ifft-output-buffer obj) 'sample
                                    (ifft-output-size obj) n)
  obj)
