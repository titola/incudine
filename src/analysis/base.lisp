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

;;; Inspired by the UAna object in Ge Wang and Perry Cook's ChucK

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:smp-ref
     incudine:free
     incudine:now)))

(defstruct (ring-buffer (:copier nil))
  (data (error "missing data for the ring buffer") :type foreign-pointer)
  (size (error "missing size for the ring buffer") :type non-negative-fixnum)
  (head 0 :type non-negative-fixnum)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(defstruct (ring-input-buffer
             (:include ring-buffer)
             (:constructor %make-ring-input-buffer (data size real-time-p))
             (:copier nil)))

(defstruct (ring-output-buffer
             (:include ring-buffer)
             (:constructor %make-ring-output-buffer (data size real-time-p))
             (:copier nil))
  ;; The slot TMP is added to avoid consing in RING-OUTPUT-BUFFER-NEXT
  (tmp (null-pointer) :type foreign-pointer))

(defun make-ring-input-buffer (size &optional real-time-p)
  (declare (type non-negative-fixnum size))
  (let ((obj (%make-ring-input-buffer
              (if real-time-p
                  (foreign-rt-alloc 'sample :count size
                                    :initial-element +sample-zero+)
                  (foreign-alloc-sample size))
              size real-time-p)))
    (when real-time-p
      (setf #1=(ring-input-buffer-foreign-free obj)
            #'safe-foreign-rt-free))
    (let ((data (ring-input-buffer-data obj))
          (foreign-free #1#))
      (tg:finalize obj (lambda () (funcall foreign-free data)))
      obj)))

(defun make-ring-output-buffer (size &optional real-time-p)
  (declare (type non-negative-fixnum size))
  (flet ((foreign-alloc (size zero-p)
           (if real-time-p
               (foreign-rt-alloc 'sample :count size :zero-p zero-p)
               (foreign-alloc-sample size))))
    (let ((obj (%make-ring-output-buffer (foreign-alloc size t) size
                                         real-time-p)))
      (setf #1=(ring-output-buffer-tmp obj) (foreign-alloc size nil))
      (when real-time-p
        (setf #2=(ring-output-buffer-foreign-free obj)
              #'safe-foreign-rt-free))
      (let ((data (ring-output-buffer-data obj))
            (tmp #1#)
            (foreign-free #2#))
        (tg:finalize obj (lambda ()
                           (funcall foreign-free data)
                           (funcall foreign-free tmp)))
        obj))))

(defmethod free ((obj ring-buffer))
  (unless (null-pointer-p #1=(ring-buffer-data obj))
    (let ((foreign-free (ring-buffer-foreign-free obj)))
      (funcall foreign-free #1#)
      (setf #1# (null-pointer))
      (when (ring-output-buffer-p obj)
        (funcall foreign-free #2=(ring-output-buffer-tmp obj))
        (setf #2# (null-pointer))))
    (tg:cancel-finalization obj))
  (setf (ring-buffer-head obj) 0
        (ring-buffer-size obj) 0)
  (values))

(declaim (inline inc-ring-buffer))
(defun inc-ring-buffer (buf delta)
  (incf (ring-buffer-head buf) delta)
  (loop while (>= (ring-buffer-head buf) (ring-buffer-size buf))
        do (decf (ring-buffer-head buf) (ring-buffer-size buf)))
  buf)

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

(defun resize-ring-buffer (obj new-size)
  (declare (type ring-buffer obj) (type positive-fixnum new-size))
  (unless (= (ring-input-buffer-size obj) new-size)
    (let ((new-data (if (ring-buffer-real-time-p obj)
                        (foreign-rt-alloc 'sample :count new-size)
                        (foreign-alloc-sample new-size)))
          (new-offset 0))
      (declare (type foreign-pointer new-data)
               (type non-negative-fixnum new-offset))
      (cond ((< new-size (ring-buffer-size obj))
             (copy-from-ring-buffer new-data obj new-size))
            (t (copy-from-ring-buffer new-data obj (ring-buffer-size obj))
               (setf new-offset (ring-buffer-size obj))))
      (foreign-free (ring-buffer-data obj))
      (setf (ring-buffer-data obj) new-data
            (ring-buffer-head obj) new-offset
            (ring-buffer-size obj) new-size)))
  new-size)

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

(defstruct (analysis (:copier nil))
  (size 0 :type non-negative-fixnum)
  (input-buffer (error "missing INPUT-BUFFER") :type foreign-pointer)
  (input-size 0 :type non-negative-fixnum)
  (output-buffer (error "missing OUTPUT-BUFFER") :type foreign-pointer)
  (output-size 0 :type non-negative-fixnum)
  (output-complex-p nil :type boolean)
  (nbins 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (time-ptr (error "missing TIME-PTR") :type foreign-pointer)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

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
  obj)

(define-constant +fftw-measure+ 0)
(define-constant +fftw-patient+ (ash 1 5))
(define-constant +fftw-estimate+ (ash 1 6))

(define-constant +fft-plan-optimal+ +fftw-patient+
  :documentation "Slowest computation of an optimal FFT plan.")

(define-constant +fft-plan-best+ +fftw-measure+
  :documentation "Slow computation of an accurate FFT plan.")

(define-constant +fft-plan-fast+ +fftw-estimate+
  :documentation "Fast computation of a reasonable FFT plan.")

(defstruct (fft-plan (:constructor %new-fft-plan))
  (pair (error "missing FFT plans") :type cons)
  (size 8 :type positive-fixnum)
  (flags +fft-plan-best+ :type fixnum))

(defstruct (fft-common (:include analysis) (:copier nil))
  (ring-buffer (error "missing RING-BUFFER") :type ring-buffer)
  (window-buffer (error "missing WINDOW-BUFFER") :type foreign-pointer)
  (window-size 0 :type non-negative-fixnum)
  (window-function (error "missing WINDOW-FUNCTION") :type function)
  (plan-wrap (error "missing FFT plan wrapper") :type fft-plan)
  (plan (error "missing FFT plan") :type foreign-pointer))

(defstruct (fft (:include fft-common) (:constructor %make-fft) (:copier nil)))

(defstruct (ifft (:include fft-common) (:constructor %make-ifft) (:copier nil)))

(defstruct (abuffer (:constructor %make-abuffer)
                    (:copier nil))
  (data (error "missing data for the abuffer") :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (nbins 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (time-ptr (error "missing time-ptr pointer for the abuffer")
            :type foreign-pointer)
  (link nil :type (or analysis null))
  (coord-complex-p nil :type boolean)
  (normalized-p nil :type boolean)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(defun make-abuffer (analysis-object &optional real-time-p)
  (declare (type analysis analysis-object))
  (flet ((foreign-alloc (size zero-p)
           (if real-time-p
               (foreign-rt-alloc 'sample :count size :zero-p zero-p)
               (foreign-alloc-sample size))))
    (let* ((coord-complex-p (analysis-output-complex-p analysis-object))
           (nbins (analysis-nbins analysis-object))
           (size (if coord-complex-p
                     (* 2 nbins)
                     (analysis-size analysis-object)))
           (time-ptr (foreign-alloc 1 nil))
           (data (foreign-alloc size t)))
      (setf (smp-ref time-ptr 0) (sample -1))
      (let ((obj (%make-abuffer :data data
                    :size size
                    :nbins nbins
                    :scale-factor (analysis-scale-factor analysis-object)
                    :time-ptr time-ptr
                    :link analysis-object
                    :coord-complex-p coord-complex-p
                    :real-time-p real-time-p)))
        (when real-time-p
          (setf #1=(abuffer-foreign-free obj)
                #'safe-foreign-rt-free))
        (let ((foreign-free #1#))
          (tg:finalize obj (lambda ()
                             (funcall foreign-free data)
                             (funcall foreign-free time-ptr)))
          obj)))))

(defmethod free ((obj abuffer))
  (when (plusp (abuffer-size obj))
    (mapc (abuffer-foreign-free obj)
          (list (abuffer-data obj) (abuffer-time-ptr obj)))
    (setf (abuffer-data obj) (null-pointer)
          (abuffer-time-ptr obj) (null-pointer)
          (abuffer-size obj) 0
          (abuffer-nbins obj) 0
          (abuffer-link obj) nil)
    (tg:cancel-finalization obj))
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

(defgeneric update-linked-object (obj)
  (:documentation "Utility function used within COMPUTE-ABUFFER to update
the linked object."))

(defmethod update-linked-object ((obj t))
  (declare (ignore obj))
  nil)

(defun compute-abuffer (abuf)
  (declare (type abuffer abuf))
  (when (< (abuffer-time abuf) (now))
    (let ((link (abuffer-link abuf)))
      (update-linked-object link)
      (setf (abuffer-coord-complex-p abuf) (analysis-output-complex-p link))
      (setf (abuffer-time abuf) (now))
      (foreign-copy (abuffer-data abuf)
                    (analysis-output-buffer link)
                    (the non-negative-fixnum
                         (* (abuffer-size abuf) +foreign-sample-size+)))))
  abuf)

(declaim (inline touch-abuffer))
(defun touch-abuffer (obj)
  (declare (type abuffer obj))
  (if (< (abuffer-time obj) (now))
      (setf (abuffer-time obj) (now)))
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
