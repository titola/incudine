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

(defvar *fft-default-window-function* (incudine.gen:sine-window)
  "Default function of two arguments called to fill an analysis data window.
The function arguments are the foreign pointer to the data window of type
SAMPLE and the window size respectively.

Initially this is set to (GEN:SINE-WINDOW).")
(declaim (type function *fft-default-window-function*))

(defvar *fft-plan* (make-hash-table :size 16))
(declaim (type hash-table *fft-plan*))

(declaim (inline get-fft-plan))
(defun get-fft-plan (size)
  "Return the cached FFT-PLAN with the specified SIZE if it exists.
Otherwise, return NIL."
  (values (gethash size *fft-plan*)))

(declaim (inline add-fft-plan))
(defun add-fft-plan (size pair)
  (setf (gethash size *fft-plan*) pair))

(declaim (inline remove-fft-plan))
(defun remove-fft-plan (size)
  "Remove the cached FFT-PLAN with the specified SIZE."
  (remhash size *fft-plan*))

(defmethod print-object ((obj fft-plan) stream)
  (format stream "#<FFT-PLAN :SIZE ~D :FLAGS ~D>"
          (fft-plan-size obj) (fft-plan-flags obj)))

(defun fft-plan-list (&optional only-size-p)
  "Return the list of the cached FFT-PLAN instances.

If ONLY-SIZE-P is T, return the sizes of these planners."
  (if only-size-p
      (sort (loop for size being the hash-keys in *fft-plan*
                  collect size)
            #'<)
      (sort (loop for plan being the hash-values in *fft-plan*
                  collect plan)
            #'< :key #'fft-plan-size)))

;;; Return a CONS where the CAR is the plan for a FFT and the CDR is
;;; the plan for a IFFT.
(defun compute-fft-plan (size flags realtime-p)
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
    (incudine.util::finalize
      (add-fft-plan size (%new-fft-plan :pair pair :size size :flags flags))
      (lambda ()
        (fft-destroy-plan (car pair))
        (fft-destroy-plan (cdr pair))))))

(defun new-fft-plan (size &optional flags)
  "Calculate and cache a new FFT-PLAN structure with the specified size
if necessary.

FLAGS is usually +FFT-PLAN-OPTIMAL+, +FFT-PLAN-BEST+ or +FFT-PLAN-FAST+.
If a cached FFT-PLAN with the same size already exists, return it if the
current thread is the real-time thread, otherwise recompute the plan if
FLAGS is non-NIL or the cached FFT-PLAN is not the best. Without a cached
FFT-PLAN, FLAGS defaults to +FFT-PLAN-FAST+."
  (declare (type positive-fixnum size) (type (or fixnum null) flags))
  (let ((plan (get-fft-plan size))
        (realtime-p (rt-thread-p)))
    (declare #.*standard-optimize-settings*)
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
                #3=(fft-common-window-buffer obj)
                #4=(analysis-time-ptr obj)))
    (incudine-cancel-finalization obj)
    (setf (fft-common-size obj) 0)
    (setf (fft-common-window-size obj) 0)
    (setf #1# (null-pointer) #2# (null-pointer)
          #3# (null-pointer) #4# (null-pointer))
    (free (fft-common-ring-buffer obj))
    (if (fft-common-real-time-p obj)
        (incudine.util::free-rt-object obj
          (if (fft-p obj) *rt-fft-pool* *rt-ifft-pool*))
        (incudine.util::free-object obj
          (if (fft-p obj) *fft-pool* *ifft-pool*)))
    (incudine.util:nrt-msg debug "Free ~A" (type-of obj))
    (values)))

(defmethod print-object ((obj fft) stream)
    (format stream "#<FFT :SIZE ~D :WINDOW-SIZE ~D :NBINS ~D>"
            (fft-size obj) (fft-window-size obj) (fft-nbins obj)))

(defun make-fft (size &key (window-size 0)
                 (window-function *fft-default-window-function*)
                 flags (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new FFT structure with the specified SIZE.

WINDOW-SIZE is the size of the analysis window and defaults to SIZE.

WINDOW-FUNCTION is NIL or a function of two arguments called to fill the
FFT data window. The function arguments are the foreign pointer to the
data window of type SAMPLE and the window size respectively.
If WINDOW-FUNCTION is NIL, the window is initially rectangular but
we can call FFT-WINDOW to get the pointer to the buffer and directly
manipulate the sample values.
WINDOW-FUNCTION defaults to *FFT-DEFAULT-WINDOW-FUNCTION*.

The argument FLAGS for the FFT planner is usually +FFT-PLAN-OPTIMAL+,
+FFT-PLAN-BEST+ or +FFT-PLAN-FAST+. If a cached FFT-PLAN with the same size
already exists, return it if the current thread is the real-time thread,
otherwise recompute the plan if FLAGS is non-NIL or the cached FFT-PLAN is
not the best. Without a cached FFT-PLAN, FLAGS defaults to +FFT-PLAN-FAST+.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type positive-fixnum size) (type non-negative-fixnum window-size)
           (type (or fixnum null) flags)
           (type (or function null) window-function)
           #.*reduce-warnings*)
  (flet ((foreign-alloc (size fftw-array-p rt-p)
           (cond (rt-p (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size) (> window-size size))
      (setf window-size size))
    (let* ((%size size)
           (winsize window-size)
           (winfunc window-function)
           (%nbins (1+ (ash size -1)))
           (complex-array-size (* 2 %nbins))
           (rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
           (%plan-wrap (new-fft-plan size flags))
           (inbuf nil)
           (outbuf nil)
           (winbuf nil)
           (tptr nil))
      (declare (type positive-fixnum %nbins complex-array-size))
      (multiple-value-bind (obj free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-fft-pool*)
                      #'safe-foreign-rt-free
                      *rt-fft-pool*)
              (values (incudine.util::alloc-rt-object *fft-pool*)
                      #'foreign-free
                      *fft-pool*))
        (declare (type fft obj) (type incudine-object-pool pool))
        (handler-case
            (progn
              (setf inbuf (foreign-alloc size t rt-p))
              (setf outbuf (foreign-alloc complex-array-size t rt-p))
              (setf winbuf (fill-window-buffer
                             (foreign-alloc window-size nil rt-p)
                             (or window-function #'rectangular-window)
                             window-size (null window-function)))
              (setf tptr (foreign-alloc 1 nil rt-p))
              (incudine.util::with-struct-slots
                  ((size input-buffer input-buffer-size output-buffer
                    output-buffer-size ring-buffer window-buffer window-size
                    window-function nbins output-complex-p scale-factor
                    time-ptr real-time-p foreign-free plan-wrap plan)
                   obj fft "INCUDINE.ANALYSIS")
                (setf size %size
                      input-buffer inbuf
                      input-buffer-size %size
                      output-buffer outbuf
                      output-buffer-size complex-array-size
                      ring-buffer (make-ring-input-buffer %size rt-p)
                      window-buffer winbuf
                      window-size winsize
                      window-function winfunc
                      nbins %nbins
                      output-complex-p t
                      scale-factor (/ (sample 1) %size)
                      time-ptr tptr
                      real-time-p rt-p
                      foreign-free free-fn
                      plan-wrap %plan-wrap
                      plan (car (fft-plan-pair %plan-wrap)))
                (setf (analysis-time obj) #.(sample -1))
                (foreign-zero-sample inbuf %size)))
          (condition (c)
            (dolist (p (list inbuf outbuf winbuf tptr))
              (if p (funcall free-fn p)))
            (incudine-object-pool-expand pool 1)
            (error c)))
        (incudine-finalize obj
          (lambda ()
            (mapc free-fn (list inbuf outbuf winbuf tptr))
            (incudine-object-pool-expand pool 1)))))))

(defun compute-fft (obj &optional force-p)
  "Compute a fast Fourier transform on the input data of the FFT
structure OBJ and write the results into the FFT output buffer.

If FORCE-P is NIL (default), the transform is computed once for the
current time."
  (declare (type fft obj) (type boolean force-p))
  (when (or force-p (fft-input-changed-p obj))
    (let ((fftsize (fft-size obj))
          (winsize (fft-window-size obj)))
      (copy-from-ring-buffer
        (fft-input-buffer obj) (fft-ring-buffer obj) fftsize)
      (unless (eq (fft-window-function obj) #'rectangular-window)
        (apply-window (fft-input-buffer obj) (fft-window-buffer obj) winsize))
      (apply-zero-padding (fft-input-buffer obj) winsize fftsize)
      (unless (zerop (fft-shift obj))
        (incudine::foreign-circular-shift (fft-input-buffer obj) 'sample
                                          (fft-input-buffer-size obj)
                                          (fft-shift obj))
        ;; The reset is a little overhead but we are applying the delayed
        ;; method CIRCULAR-SHIFT and the code is unclear if we call that
        ;; method one time for all the transforms. The alternative is to
        ;; add a keyword argument :CIRCULAR-SHIFT or :ZERO-PHASE-P to
        ;; MAKE-FFT but the lazy method seems more flexible.
        (setf (fft-shift obj) 0))
      (fft-execute (fft-plan obj) (fft-input-buffer obj) (fft-output-buffer obj))
      (setf (analysis-time obj) (now))
      (setf (fft-input-changed-p obj) nil)))
  obj)

(defmethod update-linked-object ((obj fft) force-p)
  (compute-fft obj force-p))

(defun fft-window (obj)
  "Return the foreign pointer to the sample values of the FFT window
and the FFT window size."
  (values (fft-window-buffer obj) (fft-window-size obj)))

(declaim (inline fft-input))
(defun fft-input (fft)
  "Return the sample value of the current FFT input. Setfable.

The FFT structure uses a ring buffer internally and the input buffer
is filled during the transform."
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

(defmethod circular-shift ((obj fft) n)
  ;; Lazy operation: the shift is applied during COMPUTE-FFT.
  (setf (fft-shift obj) n)
  obj)

(defun make-ifft (size &key (window-size 0)
                  (window-function *fft-default-window-function*)
                  flags (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new IFFT structure with the specified SIZE.

WINDOW-SIZE is the size of the analysis window and defaults to SIZE.

WINDOW-FUNCTION is NIL or a function of two arguments called to fill the
FFT data window. The function arguments are the foreign pointer to the
data window of type SAMPLE and the window size respectively.
If WINDOW-FUNCTION is NIL, the window is initially rectangular but
we can call IFFT-WINDOW to get the pointer to the buffer and directly
manipulate the sample values.
WINDOW-FUNCTION defaults to *FFT-DEFAULT-WINDOW-FUNCTION*.

The argument FLAGS for the FFT planner is usually +FFT-PLAN-OPTIMAL+,
+FFT-PLAN-BEST+ or +FFT-PLAN-FAST+. If a cached FFT-PLAN with the same size
already exists, return it if the current thread is the real-time thread,
otherwise recompute the plan if FLAGS is non-NIL or the cached FFT-PLAN is
not the best. Without a cached FFT-PLAN, FLAGS defaults to +FFT-PLAN-FAST+.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type positive-fixnum size) (type non-negative-fixnum window-size)
           (type (or fixnum null) flags)
           (type (or function null) window-function)
           #.*reduce-warnings*)
  (flet ((foreign-alloc (size fftw-array-p rt-p)
           (cond (rt-p (foreign-rt-alloc 'sample :count size))
                 (fftw-array-p (foreign-alloc-fft size))
                 (t (foreign-alloc-sample size)))))
    (when (or (zerop window-size) (> window-size size))
      (setf window-size size))
    (let* ((%size size)
           (winsize window-size)
           (winfunc window-function)
           (%nbins (1+ (ash size -1)))
           (complex-array-size (* 2 %nbins))
           (rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
           (%plan-wrap (new-fft-plan size flags))
           (inbuf nil)
           (outbuf nil)
           (winbuf nil)
           (tptr nil))
      (declare (type positive-fixnum %nbins complex-array-size))
      (multiple-value-bind (obj free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-ifft-pool*)
                      #'safe-foreign-rt-free
                      *rt-ifft-pool*)
              (values (incudine.util::alloc-rt-object *ifft-pool*)
                      #'foreign-free
                      *ifft-pool*))
        (declare (type ifft obj) (type incudine-object-pool pool))
        (handler-case
            (progn
              (setf inbuf (foreign-alloc complex-array-size t rt-p))
              (setf outbuf (foreign-alloc size t rt-p))
              (setf winbuf (fill-window-buffer
                             (foreign-alloc window-size nil rt-p)
                             (or window-function #'rectangular-window)
                             window-size (null window-function)))
              (setf tptr (foreign-alloc 1 nil rt-p))
              (incudine.util::with-struct-slots
                  ((size input-buffer input-buffer-size output-buffer
                    output-buffer-size ring-buffer window-buffer window-size
                    window-function nbins time-ptr real-time-p foreign-free
                    plan-wrap plan)
                   obj ifft "INCUDINE.ANALYSIS")
                (setf size %size
                      input-buffer inbuf
                      input-buffer-size complex-array-size
                      output-buffer outbuf
                      output-buffer-size %size
                      ring-buffer (make-ring-output-buffer %size rt-p)
                      window-buffer winbuf
                      window-size winsize
                      window-function winfunc
                      nbins %nbins
                      time-ptr tptr
                      real-time-p rt-p
                      foreign-free free-fn
                      plan-wrap %plan-wrap
                      plan (cdr (fft-plan-pair %plan-wrap)))
                (setf (analysis-time obj) #.(sample -1))
                (foreign-zero-sample inbuf complex-array-size)))
          (condition (c)
            (dolist (p (list inbuf outbuf winbuf tptr))
              (if p (funcall free-fn p)))
            (incudine-object-pool-expand pool 1)
            (error c)))
        (incudine-finalize obj
          (lambda ()
            (mapc free-fn (list inbuf outbuf winbuf tptr))
            (incudine-object-pool-expand pool 1)))))))

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
  "Compute an inverse fast Fourier transform on the input data of the
IFFT structure OBJ and write the results into the IFFT output buffer.

If ABUFFER is non-NIL, update that abuffer and copy the results into
the IFFT input buffer before to calculate the transform.

If FORCE-P is NIL (default), the transform is computed once for the
current time."
  (declare (type ifft obj) (type (or abuffer null) abuffer)
           (type boolean force-p))
  (when (or force-p
            (and abuffer (< (analysis-time obj) (now)))
            (ifft-input-changed-p obj))
    (when abuffer
      (compute-abuffer abuffer)
      (abuffer-complex abuffer)
      (foreign-copy-samples (ifft-input-buffer obj) (abuffer-data abuffer)
                            (ifft-input-buffer-size obj)))
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

(defun ifft-window (obj)
  "Return the foreign pointer to the sample values of the IFFT window
and the IFFT window size."
  (values (ifft-window-buffer obj) (ifft-window-size obj)))

(declaim (inline ifft-output))
(defun ifft-output (ifft)
  "Return the sample value of the current IFFT output."
  (ring-output-buffer-next (ifft-ring-buffer ifft)))

(defmethod circular-shift ((obj ifft) n)
  ;; Shifting the ring buffer head is also faster but GEN:ANALYSIS
  ;; does a copy of the output buffer.
  (incudine::foreign-circular-shift (ifft-output-buffer obj) 'sample
                                    (ifft-output-buffer-size obj) n)
  obj)
