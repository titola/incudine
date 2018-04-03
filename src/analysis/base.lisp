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
  (let ((rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*)))
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
      (incudine-finalize buf
        (lambda ()
          (funcall free-fn data-ptr)
          (incudine-object-pool-expand pool 1)))
      (incudine.util::with-struct-slots
          ((data size real-time-p foreign-free)
           buf ring-input-buffer "INCUDINE.ANALYSIS")
        (setf data data-ptr
              size bufsize
              real-time-p rt-p
              foreign-free free-fn)
        buf))))

(defun make-ring-output-buffer (bufsize
                                &optional (real-time-p (incudine.util:allow-rt-memory-p)))
  (declare (type non-negative-fixnum bufsize))
  (flet ((foreign-alloc (bufsize zero-p rt-p)
           (if rt-p
               (foreign-rt-alloc 'sample :count bufsize :zero-p zero-p)
               (foreign-alloc-sample bufsize))))
    (let* ((rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*))
           (data-ptr (foreign-alloc bufsize t rt-p))
           (tmp-ptr (foreign-alloc bufsize nil rt-p)))
      (declare (type foreign-pointer data-ptr tmp-ptr))
      (multiple-value-bind (buf free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-ring-output-buffer-pool*)
                      #'safe-foreign-rt-free
                      *rt-ring-output-buffer-pool*)
              (values (incudine.util::alloc-object *ring-output-buffer-pool*)
                      #'foreign-free
                      *ring-output-buffer-pool*))
        (declare (type ring-output-buffer buf) (type incudine-object-pool pool))
        (incudine-finalize buf
          (lambda ()
            (funcall free-fn data-ptr)
            (funcall free-fn tmp-ptr)
            (incudine-object-pool-expand pool 1)))
        (incudine.util::with-struct-slots
            ((data tmp size real-time-p foreign-free)
             buf ring-output-buffer "INCUDINE.ANALYSIS")
          (setf data data-ptr
                tmp tmp-ptr
                size bufsize
                real-time-p rt-p
                foreign-free free-fn)
          buf)))))

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
  (incf (ring-buffer-head buf))
  (when (>= (ring-buffer-head buf) (ring-buffer-size buf))
    (setf (ring-buffer-head buf) 0))
  value)

(declaim (inline copy-from-ring-buffer))
(defun copy-from-ring-buffer (c-array ring-buffer items)
  (declare (type ring-buffer ring-buffer) (type foreign-pointer c-array)
           (type non-negative-fixnum items))
  (%copy-from-ring-buffer c-array (ring-buffer-data ring-buffer)
                          (ring-buffer-size ring-buffer)
                          (ring-buffer-head ring-buffer) items))

(declaim (inline copy-to-ring-output-buffer))
(defun copy-to-ring-output-buffer (buf c-array items)
  (declare (type ring-output-buffer buf) (type foreign-pointer c-array)
           (type non-negative-fixnum items))
  (%copy-to-ring-output-buffer (ring-output-buffer-data buf)
                               c-array (ring-output-buffer-size buf)
                               (ring-output-buffer-head buf) items))

(declaim (inline ring-output-buffer-next))
(defun ring-output-buffer-next (buf)
  (declare (type ring-output-buffer buf))
  ;; Temporary variable on the foreign heap
  (setf (smp-ref (ring-output-buffer-tmp buf) 0)
        #1=(smp-ref (ring-output-buffer-data buf)
                    (ring-output-buffer-head buf)))
  (setf #1# +sample-zero+)
  (incf (ring-output-buffer-head buf))
  (when (>= (ring-output-buffer-head buf) (ring-output-buffer-size buf))
    (setf (ring-output-buffer-head buf) 0))
  (smp-ref (ring-output-buffer-tmp buf) 0))

(defstruct (analysis (:include incudine-object) (:copier nil))
  (size 0 :type non-negative-fixnum)
  (input-buffer (null-pointer) :type foreign-pointer)
  (input-size 0 :type non-negative-fixnum)
  (input-changed-p nil :type boolean)
  (output-buffer (null-pointer) :type foreign-pointer)
  (output-size 0 :type non-negative-fixnum)
  (output-complex-p nil :type boolean)
  (nbins 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (time-ptr (null-pointer) :type foreign-pointer)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(declaim (inline analysis-input))
(defun analysis-input (obj &optional bytes offset)
  "Return the input buffer of OBJ after OFFSET bytes (zero by default)."
  (declare (type analysis obj) (ignore bytes)
           (type (or non-negative-fixnum null) offset))
  (let ((in (analysis-input-buffer obj)))
    (if offset (cffi:inc-pointer in offset) in)))

(declaim (inline set-analysis-input))
(defun set-analysis-input (obj data &optional bytes offset)
  "Copy BYTES bytes from DATA to the input buffer of OBJ starting
after OFFSET bytes (zero by default).  If BYTES is NIL, copy
ANALYSIS-INPUT-SIZE samples."
  (declare (type analysis obj) (type foreign-pointer data)
           (type (or non-negative-fixnum null) bytes offset))
  (let ((in (analysis-input-buffer obj)))
    (foreign-copy (if offset (cffi:inc-pointer in offset) in)
                  data (or bytes (the non-negative-fixnum
                                   (* (analysis-input-size obj)
                                      +foreign-sample-size+))))
    (setf (analysis-input-changed-p obj) t)
    data))

(defsetf analysis-input (obj &optional bytes offset) (data)
  `(set-analysis-input ,obj ,data ,bytes ,offset))

(declaim (inline analysis-data))
(defun analysis-data (obj)
  (declare (type analysis obj))
  (analysis-output-buffer obj))

(declaim (inline analysis-time))
(defun analysis-time (obj)
  (declare (type analysis obj))
  (smp-ref (analysis-time-ptr obj) 0))

(declaim (inline set-analysis-time))
(defun set-analysis-time (obj time)
  (declare (type analysis obj) (type sample time))
  (setf (smp-ref (analysis-time-ptr obj) 0) time))

(defsetf analysis-time set-analysis-time)

(declaim (inline touch-analysis))
(defun touch-analysis (obj)
  (declare (type analysis obj))
  (if (< (analysis-time obj) (now))
      (setf (analysis-time obj) (now)))
  (setf (analysis-input-changed-p obj) t)
  obj)

(declaim (inline forget-analysis))
(defun forget-analysis (obj)
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
  (pair (list nil) :type cons)
  (size 8 :type positive-fixnum)
  (flags +fft-plan-best+ :type fixnum))

(defvar *dummy-fft-plan* (%new-fft-plan))
(declaim (type fft-plan *dummy-fft-plan*))

(defun rectangular-window (c-array size)
  (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
  (dotimes (i size c-array)
    (setf (smp-ref c-array i) #.(sample 1))))

(defstruct (fft-common (:include analysis) (:copier nil))
  (ring-buffer *dummy-ring-buffer* :type ring-buffer)
  (window-buffer (null-pointer) :type foreign-pointer)
  (window-size 0 :type non-negative-fixnum)
  (window-function #'rectangular-window :type function)
  (plan-wrap *dummy-fft-plan* :type fft-plan)
  (plan (null-pointer) :type foreign-pointer))

(defstruct (fft (:include fft-common) (:constructor %make-fft) (:copier nil)))

(defstruct (ifft (:include fft-common) (:constructor %make-ifft) (:copier nil)))

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

(defgeneric window-function (obj))

(defgeneric (setf window-function) (fn obj))

(defgeneric window-size (obj))

(defgeneric (setf window-size) (size obj))

(defmethod window-function ((obj fft-common))
  (fft-common-window-function obj))

(declaim (inline fill-window-buffer))
(defun fill-window-buffer (buffer function size)
  (if (eq function #'rectangular-window)
      buffer
      (funcall function buffer size)))

(defmethod (setf window-function) ((fn function) (obj fft-common))
  (fill-window-buffer (fft-common-window-buffer obj) fn
                      (fft-common-window-size obj))
  (setf (fft-common-window-function obj) fn))

(defmethod window-size ((obj fft-common))
  (fft-common-window-size obj))

(defmethod (setf window-size) (size (obj fft-common))
  (declare (type positive-fixnum size))
  (unless (= size (fft-common-window-size obj))
    (let* ((input-buffer (fft-common-input-buffer obj))
           (output-buffer (fft-common-output-buffer obj))
           (time-ptr (fft-common-time-ptr obj))
           (foreign-free (fft-common-foreign-free obj)))
      (multiple-value-bind (window-buffer pool)
          (if (rt-thread-p)
              (values (foreign-rt-alloc 'sample :count size)
                      (if (fft-p obj) *rt-fft-pool* *rt-ifft-pool*))
              (values (foreign-alloc-sample size)
                      (if (fft-p obj) *fft-pool* *ifft-pool*)))
        (incudine-cancel-finalization obj)
        (incudine-finalize obj
          (lambda ()
            (mapc foreign-free (list input-buffer output-buffer
                                     window-buffer time-ptr))
            (incudine-object-pool-expand pool 1)))
        (funcall foreign-free (fft-common-window-buffer obj))
        (setf (fft-common-window-buffer obj) window-buffer)
        (fill-window-buffer window-buffer (fft-common-window-function obj) size)
        (setf (fft-common-window-size obj) size))))
  size)

(defstruct (abuffer (:include incudine-object)
                    (:constructor %make-abuffer)
                    (:copier nil))
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

(define-constant +abuffer-pool-initial-size+ 400)

(defvar *abuffer-pool*
  (make-incudine-object-pool +abuffer-pool-initial-size+ #'%make-abuffer nil))
(declaim (type incudine-object-pool *abuffer-pool*))

(defvar *rt-abuffer-pool*
  (make-incudine-object-pool +abuffer-pool-initial-size+ #'%make-abuffer t))
(declaim (type incudine-object-pool *rt-abuffer-pool*))

(defun make-abuffer (analysis-object
                     &optional (real-time-p (incudine.util:allow-rt-memory-p)))
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
           (data-ptr (foreign-alloc %size t rt-p))
           (tptr (foreign-alloc 1 nil rt-p)))
      (multiple-value-bind (obj free-fn pool)
          (if rt-p
              (values (incudine.util::alloc-rt-object *rt-abuffer-pool*)
                      #'safe-foreign-rt-free
                      *rt-abuffer-pool*)
              (values (incudine.util::alloc-object *abuffer-pool*)
                      #'foreign-free
                      *abuffer-pool*))
        (declare (type abuffer obj) (type incudine-object-pool pool))
        (incudine-finalize obj
          (lambda ()
            (funcall free-fn data-ptr)
            (funcall free-fn tptr)
            (incudine-object-pool-expand pool 1)))
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
          (setf (smp-ref time-ptr 0) #.(sample -1))
          obj)))))

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

(declaim (inline abuffer-polar))
(defun abuffer-polar (obj)
  (when (abuffer-coord-complex-p obj)
    (complex-to-polar (abuffer-data obj) (abuffer-nbins obj))
    (setf (abuffer-coord-complex-p obj) nil)))

(declaim (inline abuffer-complex))
(defun abuffer-complex (obj)
  (unless (abuffer-coord-complex-p obj)
    (polar-to-complex (abuffer-data obj) (abuffer-nbins obj))
    (setf (abuffer-coord-complex-p obj) t)))

(defmacro abuffer-time (obj)
  `(smp-ref (abuffer-time-ptr ,obj) 0))

(defmacro abuffer-realpart (obj nbin)
  `(mem-ref (abuffer-data ,obj) 'sample
            (the non-negative-fixnum (* ,nbin +foreign-complex-size+))))

(defmacro abuffer-imagpart (obj nbin)
  `(mem-ref (abuffer-data ,obj) 'sample
            (the non-negative-fixnum
              (+ (the non-negative-fixnum (* ,nbin +foreign-complex-size+))
                 +foreign-sample-size+))))

(defgeneric update-linked-object (obj force-p)
  (:documentation "Utility function used within COMPUTE-ABUFFER to update
the linked object."))

(defmethod update-linked-object ((obj t) force-p)
  (declare (ignore obj force-p))
  nil)

(defun compute-abuffer (abuf &optional force-p)
  (declare (type abuffer abuf) (type boolean force-p))
  (when (or force-p (< (abuffer-time abuf) (now)))
    (let ((link (abuffer-link abuf)))
      (update-linked-object link force-p)
      (setf (abuffer-coord-complex-p abuf) (analysis-output-complex-p link))
      (setf (abuffer-time abuf) (now))
      (foreign-copy-samples (abuffer-data abuf) (analysis-output-buffer link)
                            (abuffer-size abuf))))
  abuf)

(declaim (inline touch-abuffer))
(defun touch-abuffer (obj)
  (declare (type abuffer obj))
  (if (< (abuffer-time obj) (now))
      (setf (abuffer-time obj) (now)))
  obj)

(declaim (inline forget-abuffer))
(defun forget-abuffer (obj)
  (declare (type abuffer obj))
  (setf (abuffer-time obj) (1- (now)))
  obj)

;;; Iterate over the values of one or more ABUFFERs.
;;; There are two lists of ABUFFER objects: ABUFFER-SRC-LIST and
;;; ABUFFER-DEST-LIST. The only difference is that the values of
;;; the ABUFFERs in ABUFFER-SRC-LIST are converted from polar/complex
;;; to complex/polar if COORD-CHECK-P is T, and the times of the
;;; destinations are updated after the process.
;;; We can use the keyword :INIT to insert code before the loop.
(defmacro dofft ((index-var nbins-var abuffer-src-list abuffer-dest-list
                  x-var-prefix y-var-prefix &key coord-complex-p
                  (index-start 0) index-end (coord-check-p t) init result)
                 &body body)
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

;;; Iterate over the values of one or more ABUFFERs using the polar coordinates
(defmacro dofft-polar ((index-var nbins-var abuffer-src-list abuffer-dest-list
                        &key (index-start 0) index-end (coord-check-p t)
                        init result) &body body)
  `(dofft (,index-var ,nbins-var ,abuffer-src-list ,abuffer-dest-list
           "MAG" "PHASE" :coord-complex-p nil :index-start ,index-start
           :index-end ,index-end :coord-check-p ,coord-check-p :init ,init
           :result ,result)
     ,@body))

;;; Iterate over the values of one or more ABUFFERs using the complex coordinates
(defmacro dofft-complex ((index-var nbins-var abuffer-src-list abuffer-dest-list
                          &key (index-start 0) index-end (coord-check-p t)
                          init result) &body body)
  `(dofft (,index-var ,nbins-var ,abuffer-src-list ,abuffer-dest-list "RE" "IM"
           :coord-complex-p t :index-start ,index-start :index-end ,index-end
           :coord-check-p ,coord-check-p :init ,init :result ,result)
     ,@body))
