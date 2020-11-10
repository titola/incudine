;;; Incudine version of CLM
;;; Copyright (c) 2017-2020 Tito Latini
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

(in-package :cudere-clm.sys)

;;; OSCIL

(define-clm-ugen oscil sample (inc phase fm-input pm-input)
  (:instance-type oscil-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen) (pm-input :arg-name gen))
  (prog1 (sin (+ phase pm-input))
    (incf phase (+ inc fm-input))))

(defun* make-oscil ((frequency *clm-default-frequency*) (initial-phase 0))
  (funcall (cudere-clm.ugens:oscil (hz->radians frequency) initial-phase 0 0)))

(declaim (inline oscil))
(defun oscil (os &optional (fm-input 0 fm-p) (pm-input 0 pm-p))
  (when fm-p (set-oscil-fm-input os fm-input))
  (when pm-p (set-oscil-pm-input os pm-input))
  (ugen-tick os))

(silence-compiler-if-no-args oscil (os) `(ugen-tick ,os))

(mus-frequency-method-from-radians oscil)

(defmethod mus-reset ((gen oscil-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; ENV

(define-clm-ugen env sample ((list list) scaler duration offset base
                             (object (or null envelope))
                             (location non-negative-fixnum))
  (:instance-type env-instance)
  (:readers (list :name mus-data :arg-name gen :method-p t)
            (scaler :name mus-scaler :arg-name gen :method-p t)
            (duration :arg-name gen)
            (offset :name mus-offset :arg-name gen :method-p t)
            (base :name mus-increment :arg-name gen :method-p t)
            (object :arg-name gen))
  (:accessors (location :name mus-location :arg-name gen :value-name pos
                        :method-p t))
  (declare (inline envelope))
  (with ((env (breakpoints->env list :base base :scaler scaler
                                :offset offset :duration 1.0)))
    (initialize (setf object env))
    (incf location)
    (envelope env 1 duration #'identity location)))

(defun* make-env (envelope (scaler 1.0) duration (offset 0.0) base end length)
  (let ((lst (sort-breakpoint-list envelope))
        (len (or length
                 (and end (floor (1+ end)))
                 (and duration (floor (* duration *clm-srate*)))
                 (incudine-missing-arg
                   "MAKE-ENV needs either :DURATION, :END, or :LENGTH"))))
    (funcall (cudere-clm.ugens:env lst scaler (or duration (/ len *clm-srate*))
                                   offset (or base 1) nil 0))))

(declaim (inline env))
(defun env (e) (ugen-tick e))

(declaim (inline env-interp))
(defun env-interp (x env &optional base)
  (if (and base (/= base (mus-increment env)))
      (envelope-interp x (mus-data env) base)
      (envelope-at (env-object env) x)))

(defmethod mus-length ((gen env-instance))
  (sample->fixnum (* (env-duration gen) *clm-srate*)))

(defmethod mus-reset ((gen env-instance))
  (setf (mus-location gen) 0)
  gen)

;;; TABLE-LOOKUP

(define-clm-ugen table-lookup sample
    (inc phase fm-input (wave (simple-array double-float (*)))
     (interp-type mus-interp))
  (:instance-type table-lookup-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (phase :arg-name gen)
              (wave :arg-name gen)
              (wave :name mus-data :arg-name gen :method-p t))
  (:readers (interp-type :name mus-interp-type :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with ((size (length wave))
         (k (* size +rtwopi+)))
    (declare (type non-negative-fixnum size) (type sample k))
    (prog1 (mus-interpolate interp-type phase wave)
      (increment-and-wrap-phase phase (+ inc (* fm-input k)) size))))

(defun* make-table-lookup ((frequency *clm-default-frequency*)
                           (initial-phase 0.0) wave (size *clm-table-size*)
                           (type mus-interp-linear))
  (let* ((wave (or wave (make-double-float-array size)))
         (size (length wave)))
    (funcall (cudere-clm.ugens:table-lookup (* frequency (/ size *clm-srate*))
                                            (* initial-phase size +rtwopi+)
                                            0 wave type))))

(declaim (inline table-lookup))
(defun table-lookup (tl &optional (fm-input 0 fm-p))
  (when fm-p (set-table-lookup-fm-input tl fm-input))
  (ugen-tick tl))

(silence-compiler-if-no-args table-lookup (tl) `(ugen-tick ,tl))

(defmethod mus-frequency ((gen table-lookup-instance))
  (/ (* (mus-increment gen) *clm-srate*) (length (table-lookup-wave gen))))

(defmethod (setf mus-frequency) (hz (gen table-lookup-instance))
  (set-table-lookup-inc gen (* hz (/ (length (table-lookup-wave gen)) *clm-srate*)))
  hz)

(defmethod mus-length ((gen table-lookup-instance))
  (length (table-lookup-wave gen)))

(defmethod mus-phase ((gen table-lookup-instance))
  (mod (/ (* +twopi+ (table-lookup-phase gen)) (length (table-lookup-wave gen)))
       +twopi+))

(defmethod (setf mus-phase) (value (gen table-lookup-instance))
  (set-table-lookup-phase gen (* value (length (table-lookup-wave gen))
                                 +rtwopi+))
  value)

(defmethod mus-reset ((gen table-lookup-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; POLYWAVE

(define-clm-ugen polywave sample (inc phase fm-input index
                                  (wave (simple-array double-float (*)))
                                  (type (member 1 2) type)
                                  (n non-negative-fixnum))
  (:instance-type polywave-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (index :arg-name gen)
              (phase :name mus-phase :arg-name gen :method-p t)
              (wave :arg-name gen)
              (wave :name mus-data :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples (b1 b2 cx x x2 b)
    (with ((end (if (= type mus-chebyshev-first-kind) 0 1)))
      (declare (type (integer 0 1) end))
      (setf b1 +sample-zero+
            b2 +sample-zero+
            cx (* index (cos phase))
            x2 (* 2.0 cx)
            b (aref wave (1- n)))
      (loop for i from (- n 2) downto end do
              (setf b2 b1
                    b1 b
                    b (- (+ (* b1 x2) (aref wave i)) b2)))
      (setf x phase)
      (incf phase (+ inc fm-input))
      (if (= type mus-chebyshev-first-kind)
          (- b (* cx b1))
          (* b (sin x))))))

(defun* make-polywave ((frequency *clm-default-frequency*) (partials '(1 1))
                       (type mus-chebyshev-first-kind))
  (let* ((partials (coerce partials 'list))
         (n (floor (1+ (loop for p in partials by #'cddr maximize p))))
         (coeffs (make-double-array n)))
    (loop for (i v) on partials by #'cddr do
            (setf (aref coeffs (floor i)) (double v)))
    (funcall
      (cudere-clm.ugens:polywave (hz->radians frequency) 0 0 1 coeffs type n))))

(declaim (inline polywave))
(defun polywave (w &optional (fm-input 0 fm-p))
  (when fm-p (set-polywave-fm-input w fm-input))
  (ugen-tick w))

(silence-compiler-if-no-args polywave (w) `(ugen-tick ,w))

(mus-frequency-method-from-radians polywave)

(defmethod mus-length ((gen polywave-instance))
  (length (polywave-wave gen)))

(defmethod mus-reset ((gen polywave-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; POLYSHAPE

(defun* make-polyshape ((frequency *clm-default-frequency*) (initial-phase 0.0)
                        coeffs (partials '(1 1)) (type mus-chebyshev-first-kind))
  (let ((n (if coeffs
               (length coeffs)
               (1+ (loop for p in partials by #'cddr maximize p)))))
    (unless coeffs
      (setf coeffs (make-double-array n))
      (loop for (i v) on partials by #'cddr do
              (setf (aref coeffs i) (double v))))
    (funcall
      (cudere-clm.ugens:polywave (hz->radians frequency) initial-phase 0 1
                                 coeffs type n))))

(defgeneric polyshape? (gen))
(defmethod polyshape? ((gen polywave-instance)) t)
(defmethod polyshape? ((gen t)) nil)

(declaim (inline polyshape))
(defun polyshape (w &optional (index 1.0 index-p) (fm-input 0 fm-p))
  (when index-p (set-polywave-index w index))
  (when fm-p (set-polywave-fm-input w fm-input))
  (ugen-tick w))

(silence-compiler-if-no-args polyshape (w) `(ugen-tick ,w))

;;; TRIANGLE-WAVE

(define-clm-ugen triangle-wave sample (inc amp phase fm-input)
  (:instance-type triangle-wave-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples ((g (* amp (/ +half-pi+))))
    (initialize (incf phase (* 3/2 pi)))
    (prog1 (* g (- (abs (- phase pi)) +half-pi+))
      (increment-and-wrap-phase phase (+ inc fm-input)))))

(defun* make-triangle-wave ((frequency *clm-default-frequency*) (amplitude 1.0)
                            (initial-phase 0))
  (funcall (cudere-clm.ugens:triangle-wave (hz->radians frequency) amplitude
                                           initial-phase 0)))

(declaim (inline triangle-wave))
(defun triangle-wave (s &optional (fm 0 fm-p))
  (when fm-p (set-triangle-wave-fm-input s fm))
  (ugen-tick s))

(silence-compiler-if-no-args triangle-wave (s) `(ugen-tick ,s))

(mus-frequency-method-from-radians triangle-wave)

(defmethod mus-reset ((gen triangle-wave-instance))
  (setf (mus-phase gen) (* 3/2 pi))
  gen)

;;; SQUARE-WAVE

(define-clm-ugen square-wave sample (inc amp phase fm-input width)
  (:instance-type square-wave-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t)
              (width :name mus-width :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (prog1 (if (< phase width) amp +sample-zero+)
    (increment-and-wrap-phase phase (+ inc fm-input))))

(defun* make-square-wave ((frequency *clm-default-frequency*) (amplitude 1.0)
                          (initial-phase 0))
  (funcall (cudere-clm.ugens:square-wave (hz->radians frequency) amplitude
                                         initial-phase 0 pi)))

(declaim (inline square-wave))
(defun square-wave (s &optional (fm 0 fm-p))
  (when fm-p (set-square-wave-fm-input s fm))
  (ugen-tick s))

(silence-compiler-if-no-args square-wave (s) `(ugen-tick ,s))

(mus-frequency-method-from-radians square-wave)

(defmethod mus-reset ((gen square-wave-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; SAWTOOTH-WAVE

(define-clm-ugen sawtooth-wave sample (inc amp phase fm-input)
  (:instance-type sawtooth-wave-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples ((g (* amp (/ pi))))
    (prog1 (* g (- phase pi))
      (increment-and-wrap-phase phase (+ inc fm-input)))))

(defun* make-sawtooth-wave ((frequency *clm-default-frequency*) (amplitude 1.0)
                            (initial-phase pi))
  (funcall (cudere-clm.ugens:sawtooth-wave (hz->radians frequency) amplitude
                                           initial-phase 0)))

(declaim (inline sawtooth-wave))
(defun sawtooth-wave (s &optional (fm 0 fm-p))
  (when fm-p (set-sawtooth-wave-fm-input s fm))
  (ugen-tick s))

(silence-compiler-if-no-args sawtooth-wave (s) `(ugen-tick ,s))

(mus-frequency-method-from-radians sawtooth-wave)

(defmethod mus-reset ((gen sawtooth-wave-instance))
  (setf (mus-phase gen) pi)
  gen)

;;; PULSE-TRAIN

(define-clm-ugen pulse-train sample (inc amp phase fm-input)
  (:instance-type pulse-train-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples (value)
    (initialize
      (with-follow (phase)
        (if (or (minusp phase) (>= phase +twopi+))
            (setf phase (mod phase +twopi+) value amp)
            (setf value +sample-zero+))))
    (prog1 value
      (increment-and-wrap-phase phase (+ inc fm-input) +twopi+ value amp
                                +sample-zero+))))

(defun* make-pulse-train ((frequency *clm-default-frequency*) (amplitude 1.0)
                          (initial-phase +twopi+))
  (funcall (cudere-clm.ugens:pulse-train (hz->radians (abs frequency)) amplitude
                                         initial-phase 0)))

(declaim (inline pulse-train))
(defun pulse-train (s &optional (fm 0 fm-p))
  (when fm-p (set-pulse-train-fm-input s fm))
  (ugen-tick s))

(silence-compiler-if-no-args pulse-train (s) `(ugen-tick ,s))

(mus-frequency-method-from-radians pulse-train)

(defmethod mus-reset ((gen pulse-train-instance))
  (setf (mus-phase gen) +twopi+)
  gen)

;;; NCOS

(define-clm-ugen ncos sample (inc phase fm-input (n positive-fixnum) scaler)
  (:instance-type ncos-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t)
              (n :name mus-length :arg-name gen :method-p t)
              (scaler :name mus-scaler :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples ((den 0)
                 (cos5 (+ n (sample .5))))
    (initialize
      (with-follow (n) (setf scaler (/ (sample 1) n))))
    (setf den (sin (* phase .5)))
    (prog1 (if (< (abs den) 1d-14)
               (sample 1)
               (min (sample 1)
                    (* scaler
                       (- (/ (sin (* phase cos5)) (* 2 den)) .5))))
      (incf phase (+ inc fm-input)))))

(defun* make-ncos ((frequency *clm-default-frequency*) (n 1))
  (declare (type real frequency) (type positive-fixnum n))
  (funcall
    (cudere-clm.ugens:ncos (hz->radians frequency) 0 0 n 1)))

(declaim (inline ncos))
(defun ncos (cs &optional (fm 0.0 fm-p))
  (when fm-p (set-ncos-fm-input cs fm))
  (ugen-tick cs))

(silence-compiler-if-no-args ncos (cs) `(ugen-tick ,cs))

(mus-frequency-method-from-radians ncos)

(defmethod mus-reset ((gen ncos-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; NSIN

(define-clm-ugen nsin sample (inc phase fm-input (n positive-fixnum) scaler)
  (:instance-type nsin-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t)
              (n :name mus-length :arg-name gen :method-p t)
              (scaler :name mus-scaler :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with-samples ((den 0)
                 (a2 0)
                 (cos5 (+ n (sample 1))))
    (initialize
      (with-follow (n) (setf scaler (nsin-scaler n))))
    (setf a2 (* phase .5))
    (setf den (sin a2))
    (prog1 (if (< (abs den) 1d-14)
               +sample-zero+
               (* scaler
                  (/ (* (sin (* n a2)) (sin (* a2 cos5))) den)))
      (incf phase (+ inc fm-input)))))

(defun* make-nsin ((frequency *clm-default-frequency*) (n 1))
  (declare (type real frequency) (type positive-fixnum n))
  (funcall
    (cudere-clm.ugens:nsin (hz->radians frequency) 0 0 n 1)))

(declaim (inline nsin))
(defun nsin (cs &optional (fm 0.0 fm-p))
  (when fm-p (set-nsin-fm-input cs fm))
  (ugen-tick cs))

(silence-compiler-if-no-args nsin (cs) `(ugen-tick ,cs))

(mus-frequency-method-from-radians nsin)

(defmethod mus-reset ((gen nsin-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; WAVE-TRAIN

(define-clm-ugen wave-train sample (freq phase fm-input
                                    (wave (simple-array double-float (*)))
                                    (interp-type mus-interp))
  (:instance-type wave-train-instance)
  (:accessors (freq :name mus-frequency :arg-name gen :value-name hz
                    :method-p t)
              (phase :arg-name gen))
  (:readers (wave :arg-name gen)
            (wave :name mus-data :arg-name gen :method-p t)
            (interp-type :name mus-interp-type :arg-name gen :method-p t))
  (:writers (fm-input :arg-name gen))
  (with ((wave-size (length wave))
         (out-data-size (+ wave-size 2))
         (out-data (make-frame out-data-size :zero-p t))
         (out-pos 0)
         (first-time-p t)
         (result +sample-zero+)
         (next-wave-time +sample-zero+)
         (yn1 +sample-zero+)
         (fm (* fm-input *clm-srate* +rtwopi+)))
    (declare (type positive-fixnum wave-size out-data-size)
             (type non-negative-fixnum out-pos) (type boolean first-time-p)
             (type sample result next-wave-time yn1 fm))
    (initialize (setf out-pos out-data-size))
    (maybe-expand freq phase fm)
    (when (< out-pos out-data-size)
      (setf result (smp-ref out-data out-pos)))
    (incf out-pos)
    (when (>= out-pos next-wave-time)
      (if (< out-pos out-data-size)
          (loop for i from (align-foreign-buffer-with-location
                             out-data out-pos out-data-size)
                      below out-data-size do
                  (setf (smp-ref out-data i) +sample-zero+))
          (incudine.external:foreign-zero-sample out-data out-data-size))
      (loop for i below wave-size do
              (setf yn1 (mus-interpolate interp-type (+ phase i) wave
                                         wave-size yn1))
              (incf (smp-ref out-data i) yn1))
      (cond (first-time-p
             (setf first-time-p nil)
             (setf out-pos (sample->fixnum phase))
             (when (>= out-pos wave-size)
               (setf out-pos (mod out-pos wave-size)))
             (setf result (smp-ref out-data out-pos))
             (incf out-pos)
             (setf next-wave-time (/ *clm-srate* (+ freq fm))))
            (t
             (incf next-wave-time (- (/ *clm-srate* (+ freq fm)) out-pos))
             (setf out-pos 0))))
      result))

(defun* make-wave-train ((frequency *clm-default-frequency*)
                         (initial-phase 0.0) wave (size *clm-table-size*)
                         (type mus-interp-linear))
  (let* ((wave (or wave (make-double-array size)))
         (phase (if (zerop initial-phase)
                    0.0
                    (* (length wave) (mod initial-phase +twopi+) +rtwopi+))))
    (funcall (cudere-clm.ugens:wave-train frequency phase 0 wave type))))

(declaim (inline wave-train))
(defun wave-train (w &optional (fm 0 fm-p))
  (when fm-p (set-wave-train-fm-input w fm))
  (ugen-tick w))

(silence-compiler-if-no-args wave-train (w) `(ugen-tick ,w))

(defmethod mus-phase ((gen wave-train-instance))
  (mod (/ (* +twopi+ (wave-train-phase gen)) (length (wave-train-wave gen)))
       +twopi+))

(defmethod (setf mus-phase) (value (gen wave-train-instance))
  (set-wave-train-phase gen (* (* value (length (wave-train-wave gen)))
                               +rtwopi+)))

(defmethod mus-length ((gen wave-train-instance))
  (length (wave-train-wave gen)))

(defmethod mus-reset ((gen wave-train-instance))
  (funcall (ugen-reinit-function gen) (mus-frequency gen) 0 0
           (wave-train-wave gen) (mus-interp-type gen))
  gen)

;;; RAND

(define-clm-ugen rand sample (inc amp (envelope list)
                              (distribution (or (simple-array double-float (*))
                                                null))
                              sweep phase)
  (:instance-type rand-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:readers (distribution :name mus-data :arg-name gen :method-p t))
  (:writers (distribution :arg-name gen) (sweep :arg-name gen))
  (with-samples (value)
    (initialize
      (when envelope
        (setf distribution
              (reduce-warnings (inverse-integrate envelope))))
      (setf value (randomantic amp distribution)))
    (when (or (>= phase +twopi+) (minusp phase))
      (setf phase (mod phase +twopi+))
      (setf value (randomantic amp distribution)))
    (incf phase (+ inc sweep))
    value))

(defun* make-rand ((frequency *clm-default-frequency*) (amplitude 1.0)
                   envelope distribution)
  (let ((distr (ensure-double-float-random-distribution distribution)))
    (funcall (cudere-clm.ugens:rand (hz->radians (abs frequency)) amplitude
                                    envelope distr 0 0))))

(declaim (inline rand))
(defun rand (r &optional (sweep 0 sweep-p))
  (when sweep-p (set-rand-sweep r sweep))
  (ugen-tick r))

(silence-compiler-if-no-args rand (r) `(ugen-tick ,r))

(mus-frequency-method-from-radians rand)

(defmethod mus-length ((gen rand-instance))
  (length (mus-data gen)))

(defmethod (setf mus-data) (value (gen rand-instance))
  (set-rand-distribution gen (ensure-double-float-random-distribution value)))

(defmethod mus-reset ((gen rand-instance))
  (setf (mus-phase gen) +twopi+)
  gen)

;;; RAND-INTERP

(define-clm-ugen rand-interp sample (inc amp (envelope list)
                                     (distribution
                                       (or (simple-array double-float (*))
                                           null))
                                     sweep phase)
  (:instance-type rand-interp-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t))
  (:readers (distribution :name mus-data :arg-name gen :method-p t))
  (:writers (distribution :arg-name gen) (sweep :arg-name gen))
  (with-samples ((value 0)
                 (step 0)
                 (minus-amp (- amp))
                 (norm (if (zerop inc)
                           (sample 1)
                           (/ (sample 1) (ceiling (/ +twopi+ inc))))))
    (initialize
      (when envelope
        (setf distribution
              (reduce-warnings (inverse-integrate envelope))))
      (setf value (randomantic amp distribution))
      (setf step (* (- (randomantic amp distribution) value) norm))
      (decf value step))
    (with-follow (phase)
      (when (or (minusp phase) (>= phase +twopi+))
        (setf phase (mod phase +twopi+))
        (setf value (- (randomantic amp distribution) step))))
    (incf value step)
    (cond ((> value amp) (setf value amp))
          ((< value minus-amp) (setf value minus-amp)))
    (when (or (>= phase +twopi+) (minusp phase))
      (setf phase (mod phase +twopi+))
      (setf step (* (- (randomantic amp distribution) value) norm)))
    (incf phase (+ inc sweep))
    value))

(defun* make-rand-interp ((frequency *clm-default-frequency*) (amplitude 1.0)
                          envelope distribution)
  (let ((distr (ensure-double-float-random-distribution distribution)))
    (funcall (cudere-clm.ugens:rand-interp (hz->radians (abs frequency)) amplitude
                                           envelope distr 0 0))))

(declaim (inline rand-interp))
(defun rand-interp (r &optional (sweep 0 sweep-p))
  (when sweep-p (set-rand-interp-sweep r sweep))
  (ugen-tick r))

(silence-compiler-if-no-args rand-interp (r) `(ugen-tick ,r))

(mus-frequency-method-from-radians rand-interp)

(defmethod mus-length ((gen rand-interp-instance))
  (length (mus-data gen)))

(defmethod (setf mus-data) (value (gen rand-interp-instance))
  (set-rand-interp-distribution gen
    (ensure-double-float-random-distribution value)))

(defmethod mus-reset ((gen rand-interp-instance))
  (setf (mus-phase gen) +twopi+)
  gen)

;;; ONE-POLE

(define-clm-ugen one-pole sample (input a0 b1)
  (:instance-type one-pole-instance)
  (:accessors (a0 :arg-name gen) (b1 :arg-name gen))
  (:writers (input :arg-name f))
  (~ (- (* a0 input) (* b1 it))))

(defun* make-one-pole ((a0 1.0) (b1 0.0))
  (funcall (cudere-clm.ugens:one-pole 0 a0 b1)))

(declaim (inline one-pole))
(defun one-pole (f input)
  (set-one-pole-input f input)
  (ugen-tick f))

(defmethod mus-order ((gen one-pole-instance)) 1)

(defmethod mus-xcoeff ((gen one-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (if (= loc 0) (one-pole-a0 gen) 0d0))

(defmethod mus-ycoeff ((gen one-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc (1 (one-pole-b1 gen)) (0 1d0) (otherwise 0d0)))

(defmethod (setf mus-xcoeff) (val (gen one-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (if (= loc 0) (set-one-pole-a0 gen val) val))

(defmethod (setf mus-ycoeff) (val (gen one-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (if (= loc 1) (set-one-pole-b1 gen val) val))

(defmethod mus-reset ((gen one-pole-instance))
  (set-one-pole-a0 gen 0)
  (set-one-pole-b1 gen 0)
  gen)

;;; ONE-ZERO

(define-clm-ugen one-zero sample (input a0 a1)
  (:instance-type one-zero-instance)
  (:accessors (a0 :arg-name gen) (a1 :arg-name gen))
  (:writers (input :arg-name f))
  (declare (inline delay1))
  (+ (* a0 input) (* a1 (delay1 input))))

(defun* make-one-zero ((a0 1.0) (a1 0.0))
  (funcall (cudere-clm.ugens:one-zero 0 a0 a1)))

(declaim (inline one-zero))
(defun one-zero (f input)
  (set-one-zero-input f input)
  (ugen-tick f))

(defmethod mus-order ((gen one-zero-instance)) 1)

(defmethod mus-xcoeff ((gen one-zero-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc (0 (one-zero-a0 gen)) (1 (one-zero-a1 gen)) (otherwise 0d0)))

(defmethod (setf mus-xcoeff) (val (gen one-zero-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc
    (0 (set-one-zero-a0 gen val))
    (1 (set-one-zero-a1 gen val))
    (otherwise val)))

(defmethod mus-reset ((gen one-zero-instance))
  (set-one-zero-a0 gen 0)
  (set-one-zero-a1 gen 0)
  gen)

;;; TWO-POLE

(define-clm-ugen two-pole sample (input a0 b1 b2)
  (:instance-type two-pole-instance)
  (:accessors (a0 :arg-name gen) (b1 :arg-name gen) (b2 :arg-name gen))
  (:writers (input :arg-name f))
  (declare (inline delay1))
  (~ (- (* a0 input) (* b1 it) (* b2 (delay1 it)))))

(defun* make-two-pole ((a0 1.0) (b1 0.0) b2 frequency radius)
  (funcall
    (multiple-value-call #'cudere-clm.ugens:two-pole 0
      (if (or frequency radius (not b2))
          (let ((f (or frequency a0))
                (r (or radius b1)))
            (values 1.0 (* -2.0 r (cos (hz->radians f))) (* r r)))
          (values a0 b1 b2)))))

(declaim (inline two-pole))
(defun two-pole (f input)
  (set-two-pole-input f input)
  (ugen-tick f))

(defmethod mus-order ((gen two-pole-instance)) 2)

(defmethod mus-xcoeff ((gen two-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (if (= loc 0) (two-pole-a0 gen) 0d0))

(defmethod mus-ycoeff ((gen two-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc
    (1 (two-pole-b1 gen))
    (2 (two-pole-b2 gen))
    (0 1d0)
    (otherwise 0d0)))

(defmethod (setf mus-xcoeff) (val (gen two-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (if (= loc 0) (set-two-pole-a0 gen val) val))

(defmethod (setf mus-ycoeff) (val (gen two-pole-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc
    (1 (set-two-pole-b1 gen val))
    (2 (set-two-pole-b2 gen val))
    (otherwise val)))

(defmethod mus-scaler ((gen two-pole-instance))
  (sqrt (two-pole-b2 gen)))

(defmethod (setf mus-scaler) (value (gen two-pole-instance))
  (set-two-pole-b1 gen (* -2.0 value (cos (hz->radians (mus-frequency gen)))))
  (set-two-pole-b2 gen (* value value))
  value)

(defmethod mus-frequency ((gen two-pole-instance))
  (radians->hz (acos (/ (two-pole-b1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (hz (gen two-pole-instance))
  (set-two-pole-b1 gen (* -2.0 (mus-scaler gen) (cos (hz->radians hz))))
  hz)

(defmethod mus-reset ((gen two-pole-instance))
  (set-two-pole-a0 gen 0)
  (set-two-pole-b1 gen 0)
  (set-two-pole-b2 gen 0)
  gen)

;;; TWO-ZERO

(define-clm-ugen two-zero sample (input a0 a1 a2)
  (:instance-type two-zero-instance)
  (:accessors (a0 :arg-name gen) (a1 :arg-name gen) (a2 :arg-name gen))
  (:writers (input :arg-name f))
  (declare (inline delay1))
  (with-samples ((x1 (delay1 input)))
    (+ (* a0 input) (* a1 x1) (* a2 (delay1 x1)))))

(defun* make-two-zero ((a0 1.0) (a1 0.0) a2 frequency radius)
  (funcall
    (multiple-value-call #'cudere-clm.ugens:two-zero 0
      (if (or frequency radius (not a2))
          (let ((f (or frequency a0))
                (r (or radius a1)))
            (values 1.0 (* -2.0 r (cos (hz->radians f))) (* r r)))
          (values a0 a1 a2)))))

(declaim (inline two-zero))
(defun two-zero (f input)
  (set-two-zero-input f input)
  (ugen-tick f))

(defmethod mus-order ((gen two-zero-instance)) 2)

(defmethod mus-xcoeff ((gen two-zero-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc
    (0 (two-zero-a0 gen))
    (1 (two-zero-a1 gen))
    (2 (two-zero-a2 gen))
    (otherwise 0d0)))

(defmethod (setf mus-xcoeff) (val (gen two-zero-instance) loc)
  (declare (type non-negative-fixnum loc))
  (case loc
    (0 (set-two-zero-a0 gen val))
    (1 (set-two-zero-a1 gen val))
    (2 (set-two-zero-a2 gen val))
    (otherwise val)))

(defmethod mus-scaler ((gen two-zero-instance))
  (sqrt (two-zero-a2 gen)))

(defmethod (setf mus-scaler) (value (gen two-zero-instance))
  (set-two-zero-a1 gen (* -2.0 value (cos (hz->radians (mus-frequency gen)))))
  (set-two-zero-a2 gen (* value value))
  value)

(defmethod mus-frequency ((gen two-zero-instance))
  (radians->hz (acos (/ (two-zero-a1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (hz (gen two-zero-instance))
  (set-two-zero-a1 gen (* -2.0 (mus-scaler gen) (cos (hz->radians hz))))
  hz)

(defmethod mus-reset ((gen two-zero-instance))
  (set-two-zero-a0 gen 0)
  (set-two-zero-a1 gen 0)
  (set-two-zero-a2 gen 0)
  gen)

;;; FORMANT

(define-clm-ugen formant sample (input freq radius)
  (:instance-type formant-instance)
  (:accessors (freq :name mus-frequency :arg-name gen :method-p t)
              (radius :name mus-scaler :arg-name gen :method-p t))
  (:writers (input :arg-name f) (freq :arg-name gen))
  (declare (inline delay1))
  (with-samples ((rr (* radius radius))
                 (gain (* .5 (- 1 rr)))
                 (fb (* 2 radius (cos (hz->radians freq))))
                 (x0 (* gain input))
                 (x2 (delay1 (delay1 x0))))
    (~ (+ (- x0 x2) (- (* fb it) (* rr (delay1 it)))))))

(defun* make-formant ((frequency 0.0) (radius 0.0))
  (funcall (cudere-clm.ugens:formant 0 frequency radius)))

(declaim (inline formant))
(defun formant (f input &optional (freq 0 freq-p))
  (when freq-p (set-formant-freq f freq))
  (set-formant-input f input)
  (ugen-tick f))

(define-compiler-macro formant (&whole form f input &optional freq)
  (if freq form `(progn (set-formant-input ,f ,input) (ugen-tick ,f))))

(defmethod mus-order ((gen formant-instance)) 2)

(defmethod mus-reset ((gen formant-instance))
  (setf (mus-frequency gen) 0)
  (setf (mus-scaler gen) 0)
  gen)

;;; FIRMANT

(define-clm-ugen firmant sample (input freq radius)
  (:instance-type firmant-instance)
  (:accessors (freq :name mus-frequency :arg-name gen :value-name hz
                    :method-p t)
              (radius :name mus-scaler :arg-name gen :method-p t))
  (:writers (input :arg-name f) (freq :arg-name gen))
  (with-samples ((fb (* 2 (sin (* (hz->radians freq) .5))))
                 (gain (- 1 (* radius radius)))
                 (x1 0)
                 (y1 0))
    (setf x1 (~ (+ (* gain input) (* radius (- it (* fb y1))))))
    (setf y1 (* radius (+ (* fb x1) y1)))))

(defun* make-firmant ((frequency 0.0) (radius 0.0))
  (funcall (cudere-clm.ugens:firmant 0 frequency radius)))

(declaim (inline firmant))
(defun firmant (f input &optional (freq 0 freq-p))
  (when freq-p (set-firmant-freq f freq))
  (set-firmant-input f input)
  (ugen-tick f))

(define-compiler-macro firmant (&whole form f input &optional freq)
  (if freq form `(progn (set-firmant-input ,f ,input) (ugen-tick ,f))))

(defmethod mus-order ((gen firmant-instance)) 2)

(defmethod mus-reset ((gen firmant-instance))
  (setf (mus-frequency gen) 0)
  (setf (mus-scaler gen) 0)
  gen)

;;; FILTER

(define-clm-ugen filter sample (input (order positive-fixnum)
                                (xcoeffs (simple-array double-float (*)))
                                (ycoeffs (simple-array double-float (*)))
                                (state (simple-array double-float (*))))
  (:instance-type filter-instance)
  (:readers (xcoeffs :name mus-xcoeffs :arg-name gen :method-p t)
            (xcoeffs :arg-name gen)
            (ycoeffs :name mus-ycoeffs :arg-name gen :method-p t)
            (ycoeffs :arg-name gen)
            (state :name mus-data :arg-name gen :method-p t)
            (order :name mus-order :arg-name gen :method-p t)
            (order :name mus-length :arg-name gen :method-p t)
            (order :arg-name gen))
  (:writers (input :arg-name f))
  (with ((xout +sample-zero+)
         (jn (1- order)))
    (declare (type sample xout) (type non-negative-fixnum jn))
    (setf xout +sample-zero+)
    (setf (aref state 0) input)
    (loop for j from jn downto 1 do
            (incf xout (* (aref state j) (aref xcoeffs j)))
            (decf (aref state 0)
                  (* (aref state j) (aref ycoeffs j)))
            (setf (aref state j) (aref state (1- j))))
    (+ xout (* input (aref xcoeffs 0)))))

(defun* make-filter (order xcoeffs ycoeffs)
  (let* ((xlen (length xcoeffs))
         (ylen (length ycoeffs))
         (order (or order (max xlen ylen))))
    ;; Don't replace FILTER with FIR-FILTER or IIR-FILTER because
    ;; it is possible to change the coeffs "on the road".
    (cond ((< xlen order)
           (setf xcoeffs
                 (concatenate 'list xcoeffs
                              (make-list (- order xlen) :initial-element 0.0))))
          ((> xlen order)
           (setf xcoeffs (subseq xcoeffs 0 order))))
    (cond ((< ylen order)
           (setf ycoeffs
                 (if (= ylen 0)
                     (cons 1.0 (make-list (1- order) :initial-element 0.0))
                     (concatenate 'list ycoeffs
                                  (make-list (- order ylen)
                                             :initial-element 0.0)))))
          ((> ylen order)
           (setf ycoeffs (subseq ycoeffs 0 order))))
    (funcall (cudere-clm.ugens:filter 0 order
               (if (typep xcoeffs '(simple-array double-float (*)))
                   xcoeffs
                   (make-double-float-array order :initial-contents xcoeffs))
               (if (typep ycoeffs '(simple-array double-float (*)))
                   ycoeffs
                   (make-double-float-array order :initial-contents ycoeffs))
               (make-double-float-array order)))))

(declaim (inline filter))
(defun filter (f input)
  (set-filter-input f input)
  (ugen-tick f))

(defmethod mus-xcoeff ((gen filter-instance) loc)
  (if (< loc (filter-order gen))
      (aref (filter-xcoeffs gen) loc)
      0d0))

(defmethod (setf mus-xcoeff) (val (gen filter-instance) loc)
  (if (< loc (filter-order gen))
      (setf (aref (filter-xcoeffs gen) loc) (double val))
      val))

(defmethod mus-ycoeff ((gen filter-instance) loc)
  (if (< loc (filter-order gen))
      (aref (filter-ycoeffs gen) loc)
      0d0))

(defmethod (setf mus-ycoeff) (val (gen filter-instance) loc)
  (if (< loc (filter-order gen))
      (setf (aref (filter-ycoeffs gen) loc) (double val))
      val))

(defmethod mus-reset ((gen filter-instance))
  (clear-array (mus-data gen))
  gen)

;;; FIR-FILTER

(define-clm-ugen fir-filter sample (input (order positive-fixnum)
                                    (xcoeffs (simple-array double-float (*)))
                                    (state (simple-array double-float (*))))
  (:instance-type fir-filter-instance)
  (:readers (xcoeffs :name mus-xcoeffs :arg-name gen :method-p t)
            (xcoeffs :arg-name gen)
            (state :name mus-data :arg-name gen :method-p t)
            (order :name mus-order :arg-name gen :method-p t)
            (order :name mus-length :arg-name gen :method-p t)
            (order :arg-name gen))
  (:writers (input :arg-name f))
  (with ((xout +sample-zero+)
         (jn (1- order)))
    (declare (type sample xout) (type non-negative-fixnum jn))
    (setf xout +sample-zero+)
    (setf (aref state 0) input)
    (loop for j from jn downto 1 do
            (incf xout (* (aref state j) (aref xcoeffs j)))
            (setf (aref state j) (aref state (1- j))))
    (+ xout (* input (aref xcoeffs 0)))))

(defun* make-fir-filter (order xcoeffs)
  (let ((order (or order (length xcoeffs))))
    (funcall (cudere-clm.ugens:fir-filter 0 order
               (if (typep xcoeffs '(simple-array double-float (*)))
                   xcoeffs
                   (make-double-float-array order :initial-contents xcoeffs))
               (make-double-float-array order)))))

(declaim (inline fir-filter))
(defun fir-filter (f input)
  (set-fir-filter-input f input)
  (ugen-tick f))

(defmethod mus-xcoeff ((gen fir-filter-instance) loc)
  (if (< loc (fir-filter-order gen))
      (aref (fir-filter-xcoeffs gen) loc)
      0d0))

(defmethod (setf mus-xcoeff) (val (gen fir-filter-instance) loc)
  (if (< loc (fir-filter-order gen))
      (setf (aref (fir-filter-xcoeffs gen) loc) (double val))
      val))

(defmethod mus-reset ((gen fir-filter-instance))
  (clear-array (mus-data gen))
  gen)

;;; IIR-FILTER

(define-clm-ugen iir-filter sample (input (order positive-fixnum)
                                    (ycoeffs (simple-array double-float (*)))
                                    (state (simple-array double-float (*))))
  (:instance-type iir-filter-instance)
  (:readers (ycoeffs :name mus-ycoeffs :arg-name gen :method-p t)
            (ycoeffs :arg-name gen)
            (state :name mus-data :arg-name gen :method-p t)
            (order :name mus-order :arg-name gen :method-p t)
            (order :name mus-length :arg-name gen :method-p t)
            (order :arg-name gen))
  (:writers (input :arg-name f))
  (with ((jn (1- order)))
    (declare (type non-negative-fixnum jn))
    (setf (aref state 0) input)
    (loop for j from jn downto 1 do
            (decf (aref state 0) (* (aref state j) (aref ycoeffs j)))
            (setf (aref state j) (aref state (1- j))))
    (aref state 0)))

(defun* make-iir-filter (order ycoeffs)
  (let ((order (or order (length ycoeffs))))
    (funcall (cudere-clm.ugens:iir-filter 0 order
               (if (typep ycoeffs '(simple-array double-float (*)))
                   ycoeffs
                   (make-double-float-array order :initial-contents ycoeffs))
               (make-double-float-array order)))))

(declaim (inline iir-filter))
(defun iir-filter (f input)
  (set-iir-filter-input f input)
  (ugen-tick f))

(defmethod mus-ycoeff ((gen iir-filter-instance) loc)
  (if (< loc (iir-filter-order gen))
      (aref (iir-filter-ycoeffs gen) loc)
      0d0))

(defmethod (setf mus-ycoeff) (val (gen iir-filter-instance) loc)
  (if (< loc (iir-filter-order gen))
      (setf (aref (iir-filter-ycoeffs gen) loc) (double val))
      val))

(defmethod mus-reset ((gen iir-filter-instance))
  (clear-array (mus-data gen))
  gen)

;;; DELAY

(define-clm-ugen delay sample
    (input pm-input (size positive-fixnum) (max-size positive-fixnum)
     (line (simple-array double-float (*))) (interp-type mus-interp)
     (zdly boolean) scl (loc fixnum) (zloc fixnum) (yn1-ptr t))
  (:defaults 0 0 8 8 (incudine-missing-arg "Missing delay line.")
             mus-interp-none nil 0 0 0 nil)
  (:instance-type delay-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t)
              (loc :name delay-loc :arg-name gen)
              (zloc :name delay-zloc :arg-name gen)
              (scl :name mus-scaler :arg-name gen :method-p t))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (size :arg-name gen)
            (zdly :arg-name gen)
            (max-size :arg-name gen)
            (line :name mus-data :arg-name gen :method-p t)
            (line :arg-name gen)
            (interp-type :arg-name gen)
            (interp-type :name mus-interp-type :arg-name gen :method-p t)
            (yn1-ptr :arg-name gen))
  (:writers (input :arg-name d) (pm-input :arg-name gen))
  (with ((yn1 +sample-zero+)
         (dsize (if zdly max-size size)))
    (declare (type sample yn1) (type non-negative-fixnum dsize))
    (initialize
      (setf yn1-ptr (get-pointer yn1))
      (setf loc 0)
      (setf zloc (- max-size size))
      (setf scl +sample-zero+))
    (with-follow (loc)
      (when (or (minusp loc) (>= loc dsize))
        (setf loc (mod loc dsize))))
    (with-follow (zloc)
      (when (or (minusp zloc) (>= zloc max-size))
        (setf zloc 0)))
    (with-follow (size)
      (setf size (delay-fix-size size max-size interp-type))
      (cond ((or (/= interp-type mus-interp-none) (< size max-size))
             (setf zdly t)
             (setf zloc (- max-size size))
             (setf dsize max-size))
            (t
             (setf dsize size))))
    (flet ((del (i)
             (if (and (zerop pm-input)
                      (/= interp-type mus-interp-all-pass))
                 (aref line i)
                 (delay-line-interp line i pm-input dsize interp-type yn1))))
      (prog1 (if zdly (del zloc) (del loc))
        (setf (aref line loc) input)
        (when (= (incf loc) dsize) (setf loc 0))
        (when (and zdly (= (incf zloc) max-size) (setf zloc 0)))))))

(defun delay-check-args (size max-size type)
  (when (and size max-size)
    (assert (>= max-size size)))
  (let ((len (floor (or max-size size)))
        (interp-type (or type (if max-size mus-interp-linear mus-interp-none))))
    (declare (type mus-interp interp-type))
    (cond ((= interp-type mus-interp-none) nil)
          ((member interp-type (list mus-interp-linear mus-interp-sinusoidal
                                     mus-interp-all-pass))
           (assert (> len 1)))
          ((= interp-type mus-interp-lagrange)
           (assert (> len 2)))
          (t
           (assert (> len 3))))
    (values len interp-type
            (or (/= interp-type mus-interp-none) (and max-size t)))))

(defun* make-delay ((size 1) initial-contents initial-element max-size type)
  (multiple-value-bind (len interp-type zdly)
      (delay-check-args size max-size type)
    (funcall (cudere-clm.ugens:delay 0 0 (floor (or size max-size)) len
               (make-double-array len :initial-contents initial-contents
                                  :initial-element initial-element)
               interp-type zdly))))

(declaim (inline delay))
(defun delay (d input &optional (pm 0 pm-p))
  (when pm-p (set-delay-pm-input d pm))
  (set-delay-input d input)
  (ugen-tick d))

(silence-compiler-if-no-args delay (d input)
  `(progn (set-delay-input ,d ,input) (ugen-tick ,d)))

(declaim (inline tap))
(defun tap (d &optional (offset 0.0))
  (flet ((del (i len)
           (delay-line-interp (delay-line d) i offset len (delay-interp-type d)
                              (smp-ref (delay-yn1-ptr d) 0))))
    (if (delay-zdly d)
        (del (delay-zloc d) (delay-max-size d))
        (del (delay-loc d) (delay-size d)))))

(declaim (inline delay-tick))
(defun delay-tick (d input)
  (setf (aref (delay-line d) (delay-loc d)) input)
  (incf (delay-loc d))
  (when (delay-zdly d) (incf (delay-zloc d)))
  input)

(defmethod mus-reset ((gen delay-instance))
  (clear-array (delay-line gen))
  (setf (delay-loc gen) 0)
  (setf (delay-zloc gen) (- (delay-max-size gen) (delay-size gen)))
  (setf (smp-ref (delay-yn1-ptr gen) 0) 0d0)
  gen)

;;; COMB

(define-clm-ugen comb sample
    (input pm-input (size positive-fixnum) (max-size positive-fixnum)
     (line (simple-array double-float (*))) (interp-type mus-interp)
     (zdly boolean) scl (loc fixnum) (zloc fixnum) (yn1-ptr t))
  (:defaults 0 0 8 8 (incudine-missing-arg "Missing delay line.")
             mus-interp-none nil 0 0 0 nil)
  (:instance-type comb-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t)
              (scl :name mus-scaler :arg-name gen :method-p t)
              (scl :name mus-feedback :arg-name gen :method-p t))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (line :name mus-data :arg-name gen :method-p t)
            (interp-type :name mus-interp-type :arg-name gen :method-p t)
            (max-size :arg-name gen) (yn1-ptr :arg-name gen))
  (:writers (input :arg-name cflt) (pm-input :arg-name gen) (zloc :arg-name gen))
  (declare (inline cudere-clm.ugens:delay))
  (with-samples (yn1)
    (flet ((del (i len)
             (if (and (zerop pm-input) (/= interp-type mus-interp-all-pass))
                 (aref line i)
                 (delay-line-interp line i pm-input len interp-type yn1))))
      (cudere-clm.ugens:delay
        (+ input (* scl (if zdly (del zloc max-size) (del loc size))))
        pm-input size max-size line interp-type zdly 0 loc zloc yn1-ptr))))

(defun* make-comb (scaler (size 1) initial-contents initial-element max-size type)
  (multiple-value-bind (len interp-type zdly)
      (delay-check-args size max-size type)
    (funcall (cudere-clm.ugens:comb 0 0 (floor (or size max-size)) len
               (make-double-array len :initial-contents initial-contents
                                  :initial-element initial-element)
               interp-type zdly scaler))))

(declaim (inline comb))
(defun comb (cflt input &optional (pm 0 pm-p))
  (when pm-p (set-comb-pm-input cflt pm))
  (set-comb-input cflt input)
  (ugen-tick cflt))

(silence-compiler-if-no-args comb (cflt input)
  `(progn (set-comb-input ,cflt ,input) (ugen-tick ,cflt)))

(defmethod mus-reset ((gen comb-instance))
  (clear-array (mus-data gen))
  (setf (mus-location gen) 0)
  (set-comb-zloc gen (- (comb-max-size gen) (mus-length gen)))
  (setf (smp-ref (comb-yn1-ptr gen) 0) 0d0)
  gen)

;;; FILTERED-COMB

(define-clm-ugen filtered-comb sample
    (input pm-input (size positive-fixnum) (max-size positive-fixnum)
     (line (simple-array double-float (*))) (interp-type mus-interp)
     (filter ugen-instance) (zdly boolean) scl (loc fixnum) (zloc fixnum)
     (yn1-ptr t))
  (:defaults 0 0 8 8 (incudine-missing-arg "Missing delay line.")
             mus-interp-none (incudine-missing-arg "Missing filter.")
             nil 0 0 0 nil)
  (:instance-type filtered-comb-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t)
              (scl :name mus-scaler :arg-name gen :method-p t)
              (scl :name mus-feedback :arg-name gen :method-p t)
              (filter :arg-name gen))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (line :name mus-data :arg-name gen :method-p t)
            (interp-type :name mus-interp-type :arg-name gen :method-p t)
            (max-size :arg-name gen) (yn1-ptr :arg-name gen))
  (:writers (input :arg-name cflt) (pm-input :arg-name gen) (zloc :arg-name gen))
  (declare (inline cudere-clm.ugens:delay))
  (with-samples (yn1)
    (flet ((del (i len)
             (if (and (zerop pm-input) (/= interp-type mus-interp-all-pass))
                 (aref line i)
                 (delay-line-interp line i pm-input len interp-type yn1))))
      (vuglet ((filter (in)
                 (with ((in-ptr-list (multiple-value-list
                                       (ugen-control-pointer filter 0)))
                        (in-ptr (first in-ptr-list))
                        (update-fn (second in-ptr-list)))
                   (declare (type pointer in-ptr)
                            (type (or function null) update-fn))
                   (setf (smp-ref in-ptr 0) in)
                   (reduce-warnings
                     (when update-fn (funcall update-fn))
                     (ugen-tick filter)))))
        (cudere-clm.ugens:delay
          (+ input (* scl (filter (if zdly (del zloc max-size) (del loc size)))))
          pm-input size max-size line interp-type zdly 0 loc zloc yn1-ptr)))))

(defun* make-filtered-comb (scaler (size 1) initial-contents initial-element
                            max-size type filter)
  (multiple-value-bind (len interp-type zdly)
      (delay-check-args size max-size type)
    (funcall (cudere-clm.ugens:filtered-comb 0 0 (floor (or size max-size)) len
               (make-double-array len :initial-contents initial-contents
                                  :initial-element initial-element)
               interp-type filter zdly scaler))))

(declaim (inline filtered-comb))
(defun filtered-comb (cflt input &optional (pm 0 pm-p))
  (when pm-p (set-filtered-comb-pm-input cflt pm))
  (set-filtered-comb-input cflt input)
  (ugen-tick cflt))

(silence-compiler-if-no-args filtered-comb (cflt input)
  `(progn (set-filtered-comb-input ,cflt ,input) (ugen-tick ,cflt)))

(defmethod mus-reset ((gen filtered-comb-instance))
  (clear-array (mus-data gen))
  (setf (mus-location gen) 0)
  (set-filtered-comb-zloc gen (- (filtered-comb-max-size gen) (mus-length gen)))
  (setf (smp-ref (filtered-comb-yn1-ptr gen) 0) 0d0)
  (mus-reset (filtered-comb-filter gen))
  gen)

;;; NOTCH

(define-clm-ugen notch sample
    (input pm-input (size positive-fixnum) (max-size positive-fixnum)
     (line (simple-array double-float (*))) (interp-type mus-interp)
     (zdly boolean) scl (loc fixnum) (zloc fixnum) (yn1-ptr t))
  (:defaults 0 0 8 8 (incudine-missing-arg "Missing delay line.")
             mus-interp-none nil 0 0 0 nil)
  (:instance-type notch-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t)
              (scl :name mus-scaler :arg-name gen :method-p t)
              (scl :name mus-feedforward :arg-name gen :method-p t))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (line :name mus-data :arg-name gen :method-p t)
            (interp-type :name mus-interp-type :arg-name gen :method-p t)
            (max-size :arg-name gen) (yn1-ptr :arg-name gen))
  (:writers (input :arg-name cflt) (pm-input :arg-name gen) (zloc :arg-name gen))
  (declare (inline cudere-clm.ugens:delay))
  (with-samples (yn1)
    (+ (* input scl)
       (cudere-clm.ugens:delay input pm-input size max-size line interp-type
                               zdly 0 loc zloc yn1-ptr))))

(defun* make-notch (scaler (size 1) initial-contents initial-element max-size type)
  (multiple-value-bind (len interp-type zdly)
      (delay-check-args size max-size type)
    (funcall (cudere-clm.ugens:notch 0 0 (floor (or size max-size)) len
               (make-double-array len :initial-contents initial-contents
                                  :initial-element initial-element)
               interp-type zdly scaler))))

(declaim (inline notch))
(defun notch (cflt input &optional (pm 0 pm-p))
  (when pm-p (set-notch-pm-input cflt pm))
  (set-notch-input cflt input)
  (ugen-tick cflt))

(silence-compiler-if-no-args notch (cflt input)
  `(progn (set-notch-input ,cflt ,input) (ugen-tick ,cflt)))

(defmethod mus-reset ((gen notch-instance))
  (clear-array (mus-data gen))
  (setf (mus-location gen) 0)
  (set-notch-zloc gen (- (notch-max-size gen) (mus-length gen)))
  (setf (smp-ref (notch-yn1-ptr gen) 0) 0d0)
  gen)

;;; ALL-PASS

(define-clm-ugen all-pass sample
    (input pm-input (size positive-fixnum) (max-size positive-fixnum)
     (line (simple-array double-float (*))) (interp-type mus-interp)
     (zdly boolean) fb ff (loc fixnum) (zloc fixnum) (yn1-ptr t))
  (:defaults 0 0 8 8 (incudine-missing-arg "Missing delay line.")
             mus-interp-none nil 0 0 0 0 nil)
  (:instance-type all-pass-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t)
              (fb :name mus-feedback :arg-name gen :method-p t)
              (ff :name mus-feedforward :arg-name gen :method-p t))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (line :name mus-data :arg-name gen :method-p t)
            (interp-type :name mus-interp-type :arg-name gen :method-p t)
            (max-size :arg-name gen) (yn1-ptr :arg-name gen))
  (:writers (input :arg-name f) (pm-input :arg-name gen) (zloc :arg-name gen))
  (declare (inline cudere-clm.ugens:delay))
  (with-samples (yn1)
    (flet ((del (i len)
             (if (and (zerop pm-input) (/= interp-type mus-interp-all-pass))
                 (aref line i)
                 (delay-line-interp line i pm-input len interp-type yn1))))
      (with-samples ((in (+ input
                            (* fb (if zdly (del zloc max-size) (del loc size))))))
        (+ (cudere-clm.ugens:delay in pm-input size max-size line interp-type
                                   zdly 0 loc zloc yn1-ptr)
           (* ff in))))))

(defun* make-all-pass ((feedback 0) (feedforward 0) (size 1)
                       initial-contents initial-element max-size type)
  (multiple-value-bind (len interp-type zdly)
      (delay-check-args size max-size type)
    (funcall (cudere-clm.ugens:all-pass 0 0 (floor (or size max-size)) len
               (make-double-array len :initial-contents initial-contents
                                  :initial-element initial-element)
               interp-type zdly feedback feedforward))))

(declaim (inline all-pass))
(defun all-pass (f input &optional (pm 0 pm-p))
  (when pm-p (set-all-pass-pm-input f pm))
  (set-all-pass-input f input)
  (ugen-tick f))

(silence-compiler-if-no-args all-pass (f input)
  `(progn (set-all-pass-input ,f ,input) (ugen-tick ,f)))

(defmethod mus-reset ((gen all-pass-instance))
  (clear-array (mus-data gen))
  (setf (mus-location gen) 0)
  (set-all-pass-zloc gen (- (all-pass-max-size gen) (mus-length gen)))
  (setf (smp-ref (all-pass-yn1-ptr gen) 0) 0d0)
  gen)

;;; MOVING-AVERAGE

(define-clm-ugen moving-average sample
    (input (size positive-fixnum) (line (simple-array double-float (*)))
     sum (loc fixnum))
  (:defaults 0 8 (incudine-missing-arg "Missing delay line.") 0 0)
  (:instance-type moving-average-instance)
  (:accessors (size :name mus-length :arg-name gen :method-p t)
              (loc :name mus-location :arg-name gen :method-p t))
  (:readers (size :name mus-order :arg-name gen :method-p t)
            (line :name mus-data :arg-name gen :method-p t))
  (:writers (input :arg-name f) (scl :arg-name f))
  (declare (inline cudere-clm.ugens:delay))
  (with-samples ((output (cudere-clm.ugens:delay input 0 size size line
                           mus-interp-none nil 0 loc loc nil))
                 (g (/ (sample size))))
    (incf sum (- input output))
    (* sum g)))

(defun* make-moving-average (size initial-contents initial-element)
  (let ((size (floor
                (or size (and initial-contents (length initial-contents)) 1))))
    (funcall (cudere-clm.ugens:moving-average 0 size
               (make-double-array size :initial-contents initial-contents
                                  :initial-element initial-element)))))

(declaim (inline moving-average))
(defun moving-average (f input)
  (set-moving-average-input f input)
  (ugen-tick f))

(defmethod mus-reset ((gen moving-average-instance))
  (clear-array (mus-data gen))
  (setf (mus-location gen) 0)
  gen)

;;; NRXYSIN

(define-clm-ugen nrxysin sample (inc phase fm-input (n fixnum) ratio r)
  (:instance-type nrxysin-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (r :name mus-scaler :arg-name gen :method-p t))
  (:readers (phase :name mus-phase :arg-name gen :method-p t)
            (n :name mus-length :arg-name gen :method-p t)
            (ratio :name mus-offset :arg-name gen :method-p t))
  (:writers (phase :arg-name gen) (fm-input :arg-name gen))
  (with-samples ((x phase)
                 (y (* x ratio))
                 (abs-r (abs r))
                 (norm (/ (1- (expt abs-r n)) (1- abs-r))))
    (setf phase (+ x inc fm-input))
    (/ (- (sin x)
          (* r (sin (- x y)))
          (* (expt r (1+ n))
	     (- (sin (+ x (* (1+ n) y)))
		(* r (sin (+ x (* n y)))))))
       (* norm
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defun* make-nrxysin ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (funcall (cudere-clm.ugens:nrxysin (hz->radians frequency) 0 0 n ratio r)))

(declaim (inline nrxysin))
(defun nrxysin (gen &optional (fm 0 fm-p))
  (when fm-p (set-nrxysin-fm-input gen fm))
  (ugen-tick gen))

(silence-compiler-if-no-args nrxysin (gen) `(ugen-tick ,gen))

(mus-frequency-method-from-radians nrxysin)

(defmethod (setf mus-phase) (value (gen nrxysin-instance))
  (set-nrxysin-phase gen (mod value +twopi+)))

(defmethod mus-reset ((gen nrxysin-instance))
  (set-nrxysin-phase gen 0)
  gen)

;;; NRXYCOS

(define-clm-ugen nrxycos sample (inc phase fm-input (n fixnum) ratio r)
  (:instance-type nrxycos-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (r :name mus-scaler :arg-name gen :method-p t))
  (:readers (phase :name mus-phase :arg-name gen :method-p t)
            (n :name mus-length :arg-name gen :method-p t)
            (ratio :name mus-offset :arg-name gen :method-p t))
  (:writers (phase :arg-name gen) (fm-input :arg-name gen))
  (with-samples ((x phase)
                 (y (* x ratio))
                 (abs-r (abs r))
                 (norm (/ (1- (expt abs-r (1+ n))) (1- abs-r))))
    (setf phase (+ x inc fm-input))
    (/ (- (cos x)
	  (* r (cos (- x y)))
	  (* (expt r (1+ n))
	     (- (cos (+ x (* (1+ n) y)))
		(* r (cos (+ x (* n y)))))))
       (* norm
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defun* make-nrxycos ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (funcall (cudere-clm.ugens:nrxycos (hz->radians frequency) 0 0 n ratio r)))

(declaim (inline nrxycos))
(defun nrxycos (gen &optional (fm 0 fm-p))
  (when fm-p (set-nrxycos-fm-input gen fm))
  (ugen-tick gen))

(silence-compiler-if-no-args nrxycos (gen) `(ugen-tick ,gen))

(mus-frequency-method-from-radians nrxycos)

(defmethod (setf mus-phase) (value (gen nrxycos-instance))
  (set-nrxycos-phase gen (mod value +twopi+)))

(defmethod mus-reset ((gen nrxycos-instance))
  (set-nrxycos-phase gen 0)
  gen)

;;; ASYMMETRIC-FM

(define-clm-ugen asymmetric-fm sample (inc phase fm-input ratio r index)
  (:instance-type asymmetric-fm-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t)
              (phase :name mus-phase :arg-name gen :method-p t)
              (r :name mus-scaler :arg-name gen :method-p t))
  (:readers (ratio :name mus-offset :arg-name gen :method-p t))
  (:writers (index :arg-name gen) (fm-input :arg-name gen))
  (with-samples ((mth (* ratio phase))
                 (div-r (/ r))
                 (cr (* .5 (- r div-r)))
                 (sr (* .5 (+ r div-r))))
    (prog1 (* (exp (* index cr (1+ (cos mth))))
              (cos (+ phase (* sr index (sin mth)))))
      (incf phase (+ inc fm-input)))))

(defun* make-asymmetric-fm ((frequency *clm-default-frequency*)
                            (initial-phase 0.0) (r 1.0) (ratio 1.0))
  (funcall (cudere-clm.ugens:asymmetric-fm (hz->radians frequency) initial-phase
                                           0 ratio r 0)))

(declaim (inline asymmetric-fm))
(defun asymmetric-fm (af index &optional (fm 0 fm-p))
  (when fm-p (set-asymmetric-fm-fm-input af fm))
  (set-asymmetric-fm-index af index)
  (ugen-tick af))

;;; Silence compiler if no args.
(define-compiler-macro asymmetric-fm (&whole form af index &rest opt-args)
  (if opt-args
      form
      `(progn
         (set-asymmetric-fm-index ,af ,index)
         (ugen-tick ,af))))

(mus-frequency-method-from-radians asymmetric-fm)

(defmethod mus-reset ((gen asymmetric-fm-instance))
  (setf (mus-phase gen) 0)
  gen)

;;; SSB-AM

(define-clm-ugen ssb-am sample (input fm-input inc sin-phase cos-phase
                                (order positive-fixnum)
                                (xcoeffs (simple-array double-float (*)))
                                (state (simple-array double-float (*)))
                                (line (simple-array double-float (*))))
  (:instance-type ssb-am-instance)
  (:accessors (inc :arg-name gen)
              (inc :name mus-increment :arg-name gen
                   :value-name radians-per-sample :method-p t))
  (:readers (cos-phase :arg-name gen)
            (order :arg-name gen)
            (order :name mus-length :arg-name gen :method-p t)
            (order :name mus-order :arg-name gen :method-p t)
            (xcoeffs :name mus-xcoeffs :arg-name gen :method-p t)
            (xcoeffs :arg-name gen)
            (line :name mus-data :arg-name gen :method-p t))
  (:writers (input :arg-name gen) (fm-input :arg-name gen)
            (sin-phase :arg-name gen) (cos-phase :arg-name gen))
  (declare (inline cudere-clm.ugens:oscil cudere-clm.ugens:fir-filter
                   cudere-clm.ugens:delay))
  (with ((up (plusp inc))
         (flen (length xcoeffs))
         (ccos (cudere-clm.ugens:oscil inc cos-phase fm-input 0))
         (csin (cudere-clm.ugens:oscil inc sin-phase fm-input 0))
         (yh (cudere-clm.ugens:fir-filter input flen xcoeffs state))
         (yd (cudere-clm.ugens:delay input 0 order order line)))
    (declare (type boolean up) (type positive-fixnum flen)
             (type sample ccos csin yh yd))
    (+ (* ccos yd) (* csin yh))))

(defun* make-ssb-am ((frequency *clm-default-frequency*) (order 41))
  (let* ((order (logior order 1))
         (xcoeffs (make-double-float-array (* 2 (1+ order)))))
    (loop for i from (- order) to order
          for k from 0 do
            (setf (aref xcoeffs k)
                  (if (= i 0)
                      0d0
                      (let ((c (* pi i)))
                        (declare (type limited-sample c))
                        (* (/ (- 1d0 (cos c)) c)
                           (+ 54d-2 (* 46d-2 (cos (the limited-sample
                                                    (/ c order))))))))))
    (funcall (cudere-clm.ugens:ssb-am 0 0 (hz->radians (abs frequency))
                                      (if (plusp frequency) pi 0)
                                      +half-pi+
                                      order xcoeffs
                                      (make-double-array (length xcoeffs))
                                      (make-double-array order)))))

(declaim (inline ssb-am))
(defun ssb-am (gen &optional (insig 0.0 in-p) (fm 0.0 fm-p))
  (when in-p (set-ssb-am-input gen insig))
  (when fm-p (set-ssb-am-fm-input gen fm))
  (ugen-tick gen))

(define-compiler-macro ssb-am (&whole form gen &rest opt-args)
  (cond ((cdr opt-args) form)
        (opt-args
         `(progn
            (set-ssb-am-input ,gen ,(car opt-args))
            (ugen-tick ,gen)))
        (t
         `(ugen-tick ,gen))))

(mus-frequency-method-from-radians ssb-am)

(defmethod mus-phase ((gen ssb-am-instance))
  (mod (- (ssb-am-cos-phase gen) +half-pi+) +twopi+))

(defmethod (setf mus-phase) (val (gen ssb-am-instance))
  (set-ssb-am-cos-phase gen (+ val +half-pi+))
  (set-ssb-am-sin-phase gen
    (if (plusp (ssb-am-inc gen)) (+ val pi) val)))

(defmethod mus-interp-type ((gen ssb-am-instance)) mus-interp-none)

(defmethod mus-xcoeff ((gen ssb-am-instance) loc)
  (if (< loc (ssb-am-order gen))
      (aref (ssb-am-xcoeffs gen) loc)
      0d0))

(defmethod mus-reset ((gen ssb-am-instance))
  (setf (mus-phase gen) 0)
  (clear-array (mus-data gen))
  (clear-array (ssb-am-xcoeffs gen))
  gen)

;;; FILE->SAMPLE

(defstruct (file->sample (:include soundfile:input-stream)
                         (:constructor %make-file->sample)
                         (:predicate file->sample?)))

(defun* make-file->sample (file (size *clm-file-buffer-size*))
  (soundfile:open file :buffer-size size
                  :input-stream-constructor #'%make-file->sample))

(declaim (inline file->sample))
(defun file->sample (obj samp &optional (chn 0))
  (soundfile:read obj samp chn))

;;; FILE->FRAMPLE

(defstruct (file->frample (:include soundfile:input-stream)
                          (:constructor %make-file->frample)
                          (:predicate file->frample?)))

(defun* make-file->frample (file (size *clm-file-buffer-size*))
  (soundfile:open file :buffer-size size
                  :input-stream-constructor #'%make-file->frample))

(declaim (inline file->frample))
(defun file->frample (obj samp frm)
  (soundfile:read obj samp)
  (loop for i below (file->frample-channels obj)
        for j from (file->frample-buffer-index obj) do
          (setf (aref frm i) (soundfile:buffer-value obj j)))
  frm)

;;; SAMPLE->FILE

(defstruct (sample->file (:include soundfile:output-stream)
                         (:constructor %make-sample->file)
                         (:predicate sample->file?)))

(defun make-sample->file (name &optional (chans 1) (format *clm-data-format*)
                          (type *clm-header-type*) comment)
  (let ((sf (soundfile:open name :direction :output :if-exists :supersede
                            :channels chans
                            :data-format (mus-to-sf-data-format format)
                            :header-type (mus-to-sf-header-type type)
                            :buffer-size *clm-file-buffer-size*
                            :output-stream-constructor #'%make-sample->file)))
    (when comment
      (setf (soundfile:metadata sf 'comment) comment))
    sf))

(declaim (inline sample->file))
(defun sample->file (obj samp chan val)
  (soundfile:write obj samp val chan))

(defun continue-sample->file (file)
  (if (incudine.util::probe-file* file)
      (soundfile:open file :direction :output :if-exists :mix
                      :buffer-size *clm-file-buffer-size*
                      :output-stream-constructor #'%make-sample->file)
      (error 'cudere-clm-error
             :format-control "File ~S not found."
             :format-arguments (list (namestring file)))))

;;; FRAMPLE->FILE

(defstruct (frample->file (:include soundfile:output-stream)
                          (:constructor %make-frample->file)
                          (:predicate frample->file?)))

(defun make-frample->file (name &optional (chans 1) (format *clm-data-format*)
                           (type *clm-header-type*) comment)
  (let ((sf (soundfile:open name :direction :output :if-exists :supersede
                            :channels chans
                            :data-format (mus-to-sf-data-format format)
                            :header-type (mus-to-sf-header-type type)
                            :buffer-size *clm-file-buffer-size*
                            :output-stream-constructor #'%make-frample->file)))
    (when comment
      (setf (soundfile:metadata sf 'comment) comment))
    sf))

(declaim (inline frample->file))
(defun frample->file (obj samp val)
  (dotimes (i (frample->file-channels obj) val)
    (soundfile:write obj samp (aref val i) i)))

(defun continue-frample->file (file)
  (if (incudine.util::probe-file* file)
      (soundfile:open file :direction :output :if-exists :mix
                      :buffer-size *clm-file-buffer-size*
                      :output-stream-constructor #'%make-frample->file)
      (error 'cudere-clm-error
             :format-control "File ~S not found."
             :format-arguments (list (namestring file)))))

;;; READIN

(define-clm-ugen readin sample ((sf soundfile:input-stream)
                                (location #+64-bit non-negative-fixnum
                                          #-64-bit (unsigned-byte 64))
                                (channel non-negative-fixnum)
                                (direction fixnum))
  (:instance-type readin-instance)
  (:readers (sf :arg-name gen) (location :arg-name gen) (channel :arg-name gen)
            (channel :name mus-channel :arg-name gen :method-p t))
  (:accessors (location :name mus-location :arg-name gen :method-p t)
              (direction :name mus-increment :arg-name gen :method-p t))
  (with ((forward-p (plusp direction)))
    (prog1 (soundfile:read sf location channel t forward-p)
      (setf location
            (if forward-p (1+ location) (max 0 (1- location)))))))

(defun* make-readin (file channel start (direction 1) (size *clm-file-buffer-size*))
  (funcall (cudere-clm.ugens:readin
             (if (mus-input? file)
                 file
                 (soundfile:open file :buffer-size size))
             (or (and start (floor start))
                 (and (readin? file) (readin-location file))
                 0)
             (or channel
                 (and (readin? file) (readin-channel file))
                 0)
             direction)))

(declaim (inline readin))
(defun readin (rd)
  (ugen-tick rd))

(defmethod mus-file-name ((gen readin-instance))
  (mus-file-name (readin-sf gen)))

(defmethod mus-length ((gen readin-instance))
  (soundfile:frames (readin-sf gen)))

;;; LOCSIG

(defstruct (locsig (:constructor %make-locsig) (:predicate locsig?)
                   (:conc-name locs-))
  (outn-writer *output* :type (or null soundfile:output-stream))
  (revn-writer *reverb* :type (or null soundfile:output-stream))
  (outn (incudine-missing-arg "Missing locsig array outn.")
        :type (simple-array double-float (*)))
  (revn nil :type (or null (simple-array double-float (*))))
  (outf nil :type (or null (simple-array double-float (*))))
  (revf nil :type (or null (simple-array double-float (*))))
  (reverb 0.0 :type real)
  (type-interp *clm-locsig-type* :type mus-locsig-interp)
  (degree 0.0 :type real)
  (distance 1.0 :type real)
  (chans 1 :type positive-fixnum)
  (rev-chans 0 :type non-negative-fixnum))

(defun fill-locsig (arr chans degree dist scale type)
  (declare (ignore dist))
  (if (= chans 1)
      (setf (aref arr 0) (double scale))
      (let* ((deg (if (= chans 2)
                      (max 0.0 (min 90.0 degree))
                      (mod degree 360.0)))
             (degs-per-chan (if (= chans 2) 90.0 (/ 360.0 chans)))
             (pos (/ deg degs-per-chan))
             (left (floor pos))
             (right (mod (1+ left) chans))
             (frac (- pos left)))
        (flet ((set-arr (l r)
                 (setf (aref arr left) (double l))
                 (setf (aref arr right) (double r))))
          (if (= type mus-interp-linear)
              (set-arr (* scale (- 1.0 frac))
                       (* scale frac))
              (let* ((ldeg (* +half-pi+ (- 0.5 frac)))
                     (norm (/ (sqrt 2.0)))
                     (c (cos ldeg))
                     (s (sin ldeg)))
                (declare (type limited-sample ldeg))
                (set-arr (* scale norm (+ c s))
                         (* scale norm (- c s))))))))
  arr)

(defun* make-locsig ((degree 0.0) (distance 1.0) (reverb 0.0) channels
                     (type *clm-locsig-type*))
  (let ((dist (/ 1.0 (max distance 1.0)))
        (out-chans (or channels
                       (and *output* (mus-channels *output*))
                       *clm-channels*
                       1))
        (rev-chans (if *reverb* (mus-channels *reverb*) 0))
        (detour-p (null (or *output* *reverb*))))
    (%make-locsig :outn (fill-locsig (make-double-array out-chans)
                                     out-chans degree dist dist type)
                  :revn (when *reverb*
                          (fill-locsig (make-double-array rev-chans)
                                       rev-chans degree dist
                                       (* reverb (sqrt dist)) type))
                  :outf (when detour-p (make-double-array out-chans))
                  :revf (when detour-p (make-double-array rev-chans))
                  :reverb reverb :type-interp type :degree degree
                  :distance distance :chans out-chans :rev-chans rev-chans)))

(declaim (inline locsig-detour))
(defun locsig-detour (loc in-sig)
  (flet ((detour (channels outf outn)
           (dotimes (ch channels)
             (setf (aref outf ch) (* in-sig (aref outn ch))))))
    (detour (locs-chans loc) (locs-outf loc) (locs-outn loc))
    (detour (locs-rev-chans loc) (locs-revf loc) (locs-revn loc))))

(declaim (inline %locsig))
(defun %locsig (o-stream frame in-sig channels outn)
  (dotimes (ch channels)
    (soundfile:write o-stream frame (* in-sig (aref outn ch)) ch)))

(declaim (inline locsig))
(defun locsig (loc i in-sig)
  (cond ((locs-outf loc)
         (locsig-detour loc in-sig))
        (t
         (%locsig (locs-outn-writer loc) i in-sig (locs-chans loc)
                  (locs-outn loc))
         (when (locs-revn-writer loc)
           (%locsig (locs-revn-writer loc) i in-sig (locs-rev-chans loc)
                    (locs-revn loc))))))

(declaim (inline %move-locsig))
(defun %move-locsig (outn channels degree scaler type)
  (when (> channels 2)
    (dotimes (ch channels)
      (setf (aref outn ch) 0d0))
    (fill-locsig outn channels degree nil scaler type)))

(declaim (inline move-locsig))
(defun move-locsig (loc degree distance)
  (let ((dist (/ 1.0 (max distance 1.0)))
        (rev-chans (locs-rev-chans loc)))
    (when (plusp rev-chans)
      (%move-locsig (locs-revn loc) rev-chans degree
                    (* (locs-reverb loc) (sqrt dist))
                    (locs-type-interp loc)))
    (%move-locsig (locs-outn loc) (locs-chans loc) degree dist
                  (locs-type-interp loc))))

(declaim (inline locsig-ref))
(defun locsig-ref (gen chan)
  (aref (locs-outn gen) chan))

(declaim (inline locsig-set!))
(defun locsig-set! (gen chan val)
  (setf (aref (locs-outn gen) chan) (double val)))

(defsetf locsig-ref locsig-set!)

(declaim (inline locsig-reverb-ref))
(defun locsig-reverb-ref (gen chan)
  (if (locs-revn gen) (aref (locs-revn gen) chan) 0.0))

(declaim (inline locsig-reverb-set!))
(defun locsig-reverb-set! (gen chan val)
  (when (locs-revn gen)
    (setf (aref (locs-revn gen) chan) (double val)))
  val)

(defsetf locsig-reverb-ref locsig-reverb-set!)

(defun locsig-type () *clm-locsig-type*)
(defun set-locsig-type (val) (setf *clm-locsig-type* val))
(defsetf logsig-type set-locsig-type)

(defmethod mus-channels ((gen locsig))
  (length (locs-outn gen)))

(defmethod mus-length ((gen locsig))
  (length (locs-outn gen)))

(defmethod mus-data ((gen locsig))
  (locs-outn gen))

(defmethod mus-xcoeffs ((gen locsig))
  (locs-revn gen))

(defmethod mus-xcoeff ((gen locsig) index)
  (aref (locs-revn gen) index))

(defmethod mus-reset ((gen locsig))
  (when (locs-outn gen)
    (clear-floats (locs-outn gen) (locs-chans gen)))
  (when (locs-revn gen)
    (clear-floats (locs-revn gen) (locs-rev-chans gen)))
  gen)

;;; MOVE-SOUND

(defstruct (move-sound (:constructor %make-move-sound) (:predicate move-sound?)
                       (:conc-name dlocs-))
  (outn-writer *output* :type (or null soundfile:output-stream))
  (revn-writer *reverb* :type (or null soundfile:output-stream))
  (outf (incudine-missing-arg "Missing move-sound array outf.")
        :type (simple-array double-float (*)))
  (revf nil :type (or null (simple-array double-float (*))))
  (chans 1 :type positive-fixnum)
  (rev-chans 0 :type non-negative-fixnum)
  (start 0 :type non-negative-fixnum64)
  (end 0 :type non-negative-fixnum64)
  (doppler-delay (incudine-missing-arg "Missing doppler-delay.")
                 :type delay-instance)
  (doppler-env (incudine-missing-arg "Missing doppler-env.") :type env-instance)
  (rev-env nil :type (or null env-instance))
  (out-delays (incudine-missing-arg "Missing out-delays.") :type simple-vector)
  (gains (incudine-missing-arg "Missing gains.") :type simple-vector)
  (rev-gains nil :type (or simple-vector null))
  (out-map (incudine-missing-arg "Missing gains.") :type simple-vector))

(defun* make-move-sound (dlocs-list (output *output*) (revout *reverb*))
  (destructuring-bind (start end chans rev-chans &rest rest) dlocs-list
    (declare (type non-negative-fixnum start end chans rev-chans))
    (assert (< start end))
    (let ((out-map (first (last rest))))
      (unless (typep out-map 'simple-vector)
        (setf (first (last dlocs-list)) (coerce out-map 'simple-vector))))
    (apply #'%make-move-sound
           (list* :outn-writer output
                  :revn-writer revout
                  :outf (make-double-array chans)
                  :revf (when (> rev-chans 0) (make-double-array rev-chans))
                  (loop for k in '(:start :end :chans :rev-chans :doppler-delay
                                   :doppler-env :rev-env :out-delays :gains
                                   :rev-gains :out-map)
                        for v in dlocs-list
                        collect k collect v)))))

(declaim (inline move-sound-direct-signal))
(defun move-sound-direct-signal (outf chans gains out-map out-delays in-sig)
  (declare (type non-negative-fixnum chans))
  (dotimes (ch chans)
    (let ((sample (* in-sig (env (svref gains ch)))))
      (setf (aref outf (svref out-map ch))
            (if (delay? (svref out-delays ch))
                (delay (svref out-delays ch) sample)
                sample)))))

(declaim (inline move-sound-reverb))
(defun move-sound-reverb (revf chans gains env out-map samp revn-writer in-sig)
  (declare (type non-negative-fixnum chans))
  (when (and env revf)
    (let ((val (* in-sig (env env))))
      (if gains
          (if (= chans 1)
              (setf (aref revf 0) (* val (env (svref gains 0))))
              (dotimes (ch chans)
                (setf (aref revf (svref out-map ch))
                      (* val (env (svref gains ch))))))
          (setf (aref revf 0) val))))
  (when revn-writer
    (frample->file revn-writer samp revf)))

(declaim (inline move-sound))
(defun move-sound (dloc i in-sig)
  (incudine.util::with-struct-slots
      ((outn-writer revn-writer outf revf chans rev-chans start end
        doppler-delay doppler-env rev-env out-delays gains rev-gains out-map)
       dloc dlocs)
    (if (< i start)
        (progn (delay doppler-delay in-sig) in-sig)
        (let ((val (if (> i end)
                       0d0
                       (delay doppler-delay in-sig (env doppler-env)))))
          (move-sound-direct-signal outf chans gains out-map out-delays val)
          (move-sound-reverb revf rev-chans rev-gains rev-env out-map i
                             revn-writer val)
          (when outn-writer
            (frample->file outn-writer i outf))
          in-sig))))

;;; OUT-ANY

(declaim (inline out-any))
(defun out-any (loc data &optional (channel 0) o-stream)
  (soundfile:write o-stream loc data channel))

(defmacro outa (loc data &optional (o-stream '*output*))
  `(out-any ,loc ,data 0 ,o-stream))

(defmacro outb (loc data &optional (o-stream '*output*))
  `(out-any ,loc ,data 1 ,o-stream))

(defmacro outc (loc data &optional (o-stream '*output*))
  `(out-any ,loc ,data 2 ,o-stream))

(defmacro outd (loc data &optional (o-stream '*output*))
  `(out-any ,loc ,data 3 ,o-stream))

;;; IN-ANY

(declaim (inline in-any))
(defun in-any (loc channel i-stream)
  (soundfile:read i-stream
                  (if (minusp loc)
                      (+ (soundfile:frames i-stream) loc)
                      loc)
                  channel))

(defmacro ina (loc i-stream) `(in-any ,loc 0 ,i-stream))

(defmacro inb (loc i-stream) `(in-any ,loc 1 ,i-stream))

;;; SRC

(define-constant +sinc-table-density+ 2000)
(define-constant +sinc-table-pad+ 4)

(defun make-sinc-table (&optional (width *clm-src-width*)
                        gen-window-function)
  (declare (type (integer 1 #.+max-clm-sinc-width+) width)
           (type (or function null) gen-window-function))
  (let* ((size (1+ (* 2 (+ (* width +sinc-table-density+) +sinc-table-pad+))))
         (winsize (- size (* 2 +sinc-table-pad+)))
         (table (make-double-array size)))
    (with-buffer (w winsize :fill-function (or gen-window-function (gen:hanning)))
      (cffi:with-pointer-to-vector-data (data table)
        ;; Table size is an odd number so we have 1 at the center.
        (funcall (gen:sinc (+ width (/ +sinc-table-pad+ +sinc-table-density+)))
                 data size)
        (loop for i below winsize
              for j from +sinc-table-pad+ do
          (setf #1=(cffi:mem-aref data :double j)
                (* #1# (buffer-value w i))))))
    table))

(defvar *sinc-tables*
  (let ((ht (make-hash-table)))
    (setf (gethash *clm-src-width* ht) (list (make-sinc-table)))
    ht))
(declaim (type hash-table *sinc-tables*))

(defun sinc-table (&optional (width *clm-src-width*) window-beta)
  (declare (type (integer 1 #.+max-clm-sinc-width+) width)
           (type (or positive-real null) window-beta))
  (let ((tab (gethash width *sinc-tables*)))
    (or (and (eq window-beta (cdr tab)) (car tab))
        (let ((new (make-sinc-table
                     width (and window-beta (gen:kaiser window-beta)))))
          (setf (gethash width *sinc-tables*) (cons new window-beta))
          new))))

;;; SRC here is not the original version of CLM.
;;;
;;; Note: currently, pre-filtering of the input is sometimes required
;;; if the rate is greater than one. It is to fix. In practice, the
;;; current filter is good for decimation, but the upsampling requires
;;; another filter (or other polyphase filters from the same table).
;;;
;;; The conversion is performed through the convolution between the input,
;;; upsampled or downsampled by rate, and the polyphase filters obtained
;;; from the windowed sinc table. The delay between the polyphase filters
;;; is not one sample, but it depends on the rate. For example, if rate is
;;; 1.9, the phase shift is pi/5:
;;;
;;;     2 pi (1 - frac(1.9)) = 2 pi 0.1 = pi/5
;;;
;;; therefore the index of the first coefficient for the next polyphase
;;; filter is
;;;
;;;     SINC-TABLE-DENSITY * 0.1 = 2000 * 0.1 = 200
;;;
;;; The distortion is remarkably reduced and the code is simplified
;;; (but it requires pre-filtering if rate >1).
(define-clm-ugen src-tmp sample
    ((rd (or null soundfile:input-stream)) sr-change
     srate (sinc-table (simple-array double-float (*)))
     (width positive-fixnum) (input function))
  (:instance-type src-tmp-instance)
  (:readers (rd :arg-name gen) (sinc-table :arg-name gen) (width :arg-name gen))
  (:accessors (input :arg-name gen)
              (srate :name mus-increment :arg-name gen :method-p t)
              (sr-change :arg-name gen))
  (with ((x 0d0)
         (srx 0d0)
         (sum 0d0)
         (width-1 (- 1d0 width))
         (fsinc-loc 0d0)
         (fsinc-incr 0d0)
         (lim (* 2 width))
         (start 0)
         (loc 0)
         (fsx 0)
         (data (make-frame (1+ (* 2 lim)) :zero-p t))
         (int-p nil))
    (declare (type sample x srx sum  width-1 fsinc-loc fsinc-incr)
             (type fixnum lim start loc fsx)
             (type boolean int-p))
    (initialize
      rd ; ignore
      (loop for i from (1- width) below lim
            with dir = (if (minusp srate) -1 1) do
              (setf (smp-ref data i) (funcall input dir))
              (setf (smp-ref data (+ i lim)) (smp-ref data i))))
    (setf loc start)
    (nclip sr-change #.(sample (- +max-clm-src+)) #.(sample +max-clm-src+))
    (setf srx (+ srate sr-change))
    (when (>= x 1)
      (setf fsx (sample->fixnum x))
      (decf x fsx)
      (loop for i below fsx
            with dir = (if (< srx 0) -1 1) do
              (setf (smp-ref data loc) (funcall input dir))
              (setf (smp-ref data (+ loc lim)) (smp-ref data loc))
              (incf loc)
              (when (= loc lim)
                (setf loc 0)))
      (setf start loc))
    (when (< srx 0)
      (setf srx (- srx)))
    (setf int-p
          (if (> srx 1)
              (let ((zf (/ +sinc-table-density+ srx)))
                (<= (abs (* (- (sample->fixnum zf :roundp t) zf) lim)) 2))
              t))
    (setf sum +sample-zero+)
    (cond (int-p
           (loop for i of-type fixnum from loc below (+ loc lim)
                 for j of-type fixnum
                       from (+ (- +sinc-table-density+
                                  (sample->fixnum (* +sinc-table-density+ x)))
                               +sinc-table-pad+)
                       by +sinc-table-density+
                 do (incf sum (* (smp-ref data i) (aref sinc-table j)))))
          (t
           (setf fsinc-loc (sample (+ (* +sinc-table-density+ (- 1 x)) +sinc-table-pad+)))
           (setf fsinc-incr (sample +sinc-table-density+))
           (loop for i of-type fixnum from loc below (+ loc lim) do
                   (incf sum (* (smp-ref data i)
                                (aref sinc-table (sample->fixnum fsinc-loc))))
                   (incf fsinc-loc fsinc-incr))))
    (incf x srx)
    sum))

;;; From the original version of SRC.
(define-clm-ugen src sample
    ((rd (or null soundfile:input-stream)) sr-change
     srate (sinc-table (simple-array double-float (*)))
     (width positive-fixnum) (input function))
  (:instance-type src-instance)
  (:readers (rd :arg-name gen) (sinc-table :arg-name gen) (width :arg-name gen))
  (:accessors (input :arg-name gen)
              (srate :name mus-increment :arg-name gen :method-p t)
              (sr-change :arg-name gen))
  (with ((x 0d0)
         (srx 0d0)
         (factor 1d0)
         (zf 0d0)
         (sum 0d0)
         (width-1 (- 1d0 width))
         (fsinc-loc 0d0)
         (fsinc-incr 0d0)
         (lim (* 2 width))
         (start 0)
         (loc 0)
         (fsx 0)
         (xi 0)
         (sinc4 (+ (* width +sinc-table-density+) +sinc-table-pad+))
         (data (make-frame (1+ (* 2 lim)) :zero-p t))
         (int-p nil))
    (declare (type sample x srx factor zf sum  width-1 fsinc-loc fsinc-incr)
             (type fixnum lim start loc fsx xi sinc4)
             (type boolean int-p))
    (initialize
      rd ; ignore
      (loop for i from (1- width) below lim
            with dir = (if (minusp srate) -1 1) do
              (setf (smp-ref data i) (funcall input dir))
              (setf (smp-ref data (+ i lim)) (smp-ref data i))))
    (setf loc start)
    (nclip sr-change #.(sample (- +max-clm-src+)) #.(sample +max-clm-src+))
    (setf srx (+ srate sr-change))
    (when (>= x 1)
      (setf fsx (sample->fixnum x))
      (decf x fsx)
      (loop for i below fsx
            with dir = (if (< srx 0) -1 1) do
              (setf (smp-ref data loc) (funcall input dir))
              (setf (smp-ref data (+ loc lim)) (smp-ref data loc))
              (incf loc)
              (when (= loc lim)
                (setf loc 0)))
      (setf start loc))
    (when (< srx 0)
      (setf srx (- srx)))
    (cond ((> srx 1)
           (setf factor (/ srx))
           (setf zf (* factor +sinc-table-density+))
           (setf xi (round zf))
           (setf int-p (<= (abs (* (- xi zf) lim)) 2)))
          (t
           (setf factor (sample 1))
           (setf zf (sample +sinc-table-density+))
           (setf xi +sinc-table-density+)
           (setf int-p t)))
    (setf sum +sample-zero+)
    (cond (int-p
           (loop for i of-type fixnum from loc below (+ loc lim)
                 for j of-type fixnum
                       from (+ (sample->fixnum (* zf (- width-1 x))) sinc4) by xi
                 do (incf sum (* (smp-ref data i) (aref sinc-table j)))))
          (t
           (setf fsinc-loc (+ (* zf (- width-1 x)) sinc4))
           (setf fsinc-incr zf)
           (loop for i of-type fixnum from loc below (+ loc lim) do
                   (incf sum (* (smp-ref data i)
                                (aref sinc-table (sample->fixnum fsinc-loc))))
                   (incf fsinc-loc fsinc-incr))))
    (incf x srx)
    (* sum factor)))

;;; Use SRC-TYPE EXPERIMENTAL to test the new version.
;;; The parameter WINDOW-BETA for the Kaiser window doesn't work
;;; with the original SRC.
(defun* make-src (input (srate 1.0) (width *clm-src-width*) window-beta src-type)
  (declare (type (real #.(- +max-clm-src+) #.+max-clm-src+) srate)
           (type (integer 1 #.+max-clm-sinc-width+) width)
           (type (or positive-real null) window-beta)
           (type symbol src-type))
  (let* ((width (if (< width (floor (* (abs srate) 2)))
                    (* (ceiling (abs srate)) 2)
                    width))
         (rd (cond ((mus-input? input) input)
                   ((readin? input) (readin-sf input))
                   ((and (or (stringp input) (pathnamep input))
                         (dynamic-incudine-finalizer-p))
                    (make-file->sample (mus-file-name input)))))
         (func (cond ((functionp input) input)
                     ((mus-input? rd)
                      (when (and (minusp srate)
                                 (zerop (soundfile:current-frame rd)))
                        (setf (soundfile:position rd) (soundfile:frames rd)))
                      (lambda (dir)
                        (soundfile:read-next rd 0 nil (= dir 1))))
                     (t
                      (constantly 0d0)))))
    (when (and (= srate 2) (oddp width))
      (incf width))
    (funcall
      (funcall (if (string= (symbol-name src-type) "EXPERIMENTAL")
                   #'cudere-clm.ugens::src-tmp
                   ;; Original version.
                   #'cudere-clm.ugens:src)
               rd 0 srate (sinc-table width window-beta) width func))))

(declaim (inline src))
(defun src (s &optional (sr-change 0 sr-p) input-function)
  (declare (type src-instance s) (type real sr-change)
           (type (or function null)))
  (when sr-p (set-src-sr-change s sr-change))
  (when (and input-function (not (eq input-function (src-input s))))
    (set-src-input s input-function))
  (ugen-tick s))

(silence-compiler-if-no-args src (s) `(ugen-tick ,s))

(defmethod mus-location ((gen src-instance))
  (mus-location (src-rd gen)))

(defmethod (setf mus-location) (pos (gen src-instance))
  (setf (mus-location (src-rd gen)) pos))

(defmethod mus-reset ((gen src-instance))
  (funcall (ugen-reinit-function gen) (src-rd gen) (src-sr-change gen)
           (mus-increment gen) (src-sinc-table gen) (src-width gen)
           (src-input gen))
  gen)

;;; CONVOLVE

(define-clm-ugen convolve sample ((filter (simple-array double-float (*)))
                                  (fft-size positive-fixnum)
                                  (input function) (fft-instance fft))
  (:instance-type convolve-instance)
  (:readers (fft-size :name mus-length :arg-name gen :method-p t)
            (filter :name mus-xcoeffs :arg-name gen :method-p t)
            (fft-instance :arg-name gen))
  (:accessors (input :arg-name gen))
  (with ((filter-size (length filter))
         (n (ash fft-size -1))
         (ctr n)
         (fft-bytes (* fft-size +foreign-sample-size+))
         (n-bytes (ash fft-bytes -1))
         (rl1 (make-frame (* fft-size 3)))
         (rl1h (cffi:inc-pointer rl1 n-bytes))
         (rl2 (cffi:inc-pointer rl1 fft-bytes))
         (buf (cffi:inc-pointer rl2 fft-bytes))
         (bufh (cffi:inc-pointer buf n-bytes)))
    (declare (type positive-fixnum n filter-size fft-bytes n-bytes)
             (type non-negative-fixnum ctr) (type frame rl1h rl2 buf bufh))
    (initialize
      (incudine.external:foreign-zero-sample bufh n))
    (when (>= ctr n)
      (dotimes (i n)
        (setf (smp-ref buf i) (smp-ref bufh i))
        (setf (smp-ref rl1 i) (funcall input 1))
        (setf (smp-ref rl1h i) 0d0))
      (dotimes (i fft-size)
        (setf (smp-ref rl2 i)
              (if (< i filter-size) (aref filter i) 0d0)))
      (foreign-convolution rl1 rl2 fft-size fft-instance)
      (dotimes (i n)
        (incf (smp-ref buf i) (smp-ref rl1 i))
        (setf (smp-ref bufh i) (smp-ref rl1h i)))
      (setf ctr 0))
    (prog1 (smp-ref buf ctr) (incf ctr))))

(defun* make-convolve (input filter (fft-size 32) filter-size (filter-scaler 1.0)
                       (fft-instance *fft*))
  (declare (type positive-fixnum fft-size)
           (type (or positive-fixnum null) filter-size)
           (type real filter-scaler))
  (let* ((filter (cond
                   ((or (mus-input? filter) (readin? filter))
                    (let* ((filter (if (mus-input? filter)
                                       filter
                                       (readin-sf filter)))
                           (size (or filter-size (soundfile:frames filter))))
                      (file->array (mus-file-name filter) (mus-channel filter)
                                   (mus-location filter) size
                                   (make-double-array size))))
                   ((stringp filter)
                    (let ((size (or filter-size (soundfile:frames filter))))
                      (file->array filter 0 0 size (make-double-array size))))
                   ((arrayp filter)
                    filter)))
         (fft-size (max fft-size (* 2 (next-power-of-two (length filter)))))
         (rd (cond ((mus-input? input) input)
                   ((readin? input) (readin-sf input))
                   ((and (or (stringp input) (pathnamep input))
                         (dynamic-incudine-finalizer-p))
                    (make-file->sample (mus-file-name input)))))
         (func (cond ((functionp input) input)
                     ((mus-input? rd)
                      (lambda (x)
                        (declare (ignore x))
                        (soundfile:read-next rd)))
                     (t
                      (constantly 0d0)))))
    (unless (= filter-scaler 1.0)
      (dotimes (i (length filter))
        (setf (aref filter i) (* (aref filter i) filter-scaler))))
    (funcall (cudere-clm.ugens:convolve filter fft-size func fft-instance))))

(declaim (inline convolve))
(defun convolve (ff &optional input-function)
  (when (and input-function (not (eq input-function (convolve-input ff))))
    (set-convolve-input ff input-function))
  (ugen-tick ff))

(silence-compiler-if-no-args convolve (ff) `(ugen-tick ,ff))

(defun* convolve-files (file1 file2 (maxamp 1.0) (output-file "tmp.snd")
                        (fft-instance *fft*))
  (declare (type string file1 file2 output-file) (type real maxamp)
           (type fft fft-instance))
  (let ((len1 (soundfile:frames file1))
        (len2 (soundfile:frames file2)))
    (unless (or (zerop len1) (zerop len2))
      (let* ((chans1 (soundfile:channels file1))
             (chans2 (soundfile:channels file2))
             (out-chans (max chans1 chans2))
             (out-frames (+ len1 len2 1))
             (fftsize (next-power-of-two out-frames))
             (size (* out-chans out-frames)))
        (declare (type non-negative-fixnum chans1 chans2 out-chans fftsize)
                 (type non-negative-fixnum64 out-frames size))
        (with-buffers ((data1 fftsize)
                       (data2 fftsize)
                       (out size))
          (loop for ch below out-chans
                for c1 = 0 then (mod (1+ c1) chans1)
                for c2 = 0 then (mod (1+ c2) chans2) do
                  (fill-buffer data1 file1 :channel-map `((,ch 0)))
                  (fill-buffer data2 file2 :channel-map `((,ch 0)))
                  (foreign-convolution (buffer-data data1) (buffer-data data2)
                                       fftsize fft-instance)
                  (loop for j from ch below size by out-chans
                        for k from 0 do
                          (setf (buffer-value out j)
                                (buffer-value data1 k)))
                  (incudine.external:foreign-zero-sample
                    (buffer-data data1) fftsize)
                  (incudine.external:foreign-zero-sample
                    (buffer-data data2) fftsize))
          (normalize-buffer out maxamp)
          (buffer-save
            out output-file :end (+ len1 len2)
            :sample-rate (soundfile:sample-rate file1)
            :header-type #.(mus-to-sf-header-type *clm-tempfile-header-type*)
            :data-format #.(mus-to-sf-data-format *clm-tempfile-data-format*))
          output-file)))))

(defmethod mus-reset ((gen convolve-instance))
  (funcall (ugen-reinit-function gen) (mus-xcoeffs gen) (mus-length gen)
           (convolve-input gen) (convolve-fft-instance gen))
  gen)

;;; GRANULATE

(define-clm-ugen granulate sample (hop amp expansion jitter
                                   (ramp non-negative-fixnum)
                                   (max-size non-negative-fixnum)
                                   (seed (unsigned-byte 32))
                                   (input function) (edit (or function null))
                                   (grain (simple-array double-float (*)))
                                   (grain-len positive-fixnum)
                                   (output-hop non-negative-fixnum)
                                   (self (or null ugen-instance)))
  (:instance-type granulate-instance)
  (:readers (grain :name mus-data :arg-name gen :method-p t)
            (max-size :arg-name gen))
  (:accessors (hop :name mus-frequency :arg-name gen :method-p t)
              (ramp :name mus-ramp :arg-name gen :method-p t)
              (output-hop :name mus-hop :arg-name gen :method-p t)
              (amp :name mus-scaler :arg-name gen :method-p t)
              (expansion :name mus-increment :arg-name gen :method-p t)
              (jitter :name mus-offset :arg-name gen :method-p t)
              (grain-len :name mus-length :arg-name gen :method-p t)
              (seed :name mus-location :arg-name gen :method-p t)
              (input :arg-name gen) (edit :arg-name gen)
              (self :arg-name gen))
  (with ((input-hop 0)
         (result 0)
         (scl 0)
         (step (/ amp ramp))
         (ctr 0)
         (cur-out 0)
         (sx (* jitter *srate* hop))
         (s20 (* 2 (sample->fixnum sx)))
         (s50 (sample->fixnum (* sx .4)))
         (half-s50 (ash s50 -1))
         (outlen max-size)
         (inlen (+ outlen s20 1))
         (out-data (make-frame outlen :zero-p t))
         (in-data (make-frame inlen))
         (first-samp-p t)
         (random-state (sb-ext:seed-random-state seed)))
    (declare (type sample result scl step sx)
             (type fixnum input-hop ctr cur-out s20 s50 half-s50 outlen inlen)
             (type boolean first-samp-p))
    (initialize
      (setf output-hop (sample->fixnum (* hop *srate*)))
      (with-follow (expansion)
        (unless (plusp expansion)
          (setf expansion (sample 1)))
        (setf input-hop (sample->fixnum (/ output-hop expansion))))
      (with-follow (hop)
        (setf output-hop (sample->fixnum (* hop *srate*)))
        (setf input-hop (sample->fixnum (/ output-hop expansion))))
      (with-follow (ramp)
        (unless (< ramp (* grain-len .5))
          (setf ramp (1- (ash grain-len -1)))
          (setf step (/ amp ramp)))))
    (with-follow (grain-len)
      (cond ((<= grain-len 0) (setf grain-len 1))
            ((>= grain-len outlen) (setf grain-len (1- outlen)))))
    (with-follow (output-hop)
      (setf hop (sample (/ output-hop *srate*)))
      (setf input-hop (sample->fixnum (/ output-hop expansion)))
      (setf sx (* jitter *srate* hop))
      (setf s20 (* 2 (sample->fixnum sx)))
      (setf s50 (sample->fixnum (* sx .4)))
      (setf half-s50 (ash s50 -1)))
    (setf result (if (< ctr outlen) (smp-ref out-data ctr) +sample-zero+))
    (incf ctr)
    (when (>= ctr cur-out)
      (cond (first-samp-p
             (dotimes (i inlen)
               (setf (smp-ref in-data i) (funcall input 1))))
            (t
             (if (>= cur-out outlen)
                 (incudine.external:foreign-zero-sample out-data outlen)
                 (loop for i from (align-foreign-buffer-with-location
                                    out-data cur-out outlen)
                             below outlen do
                         (setf (smp-ref out-data i) +sample-zero+)))
             (cond ((> input-hop inlen)
                    (loop for i from inlen below input-hop do
                            (funcall input 1))
                    (dotimes (i inlen)
                      (setf (smp-ref in-data i) (funcall input 1))))
                   (t
                    (loop for i from (align-foreign-buffer-with-location
                                       in-data input-hop inlen)
                                below inlen do
                            (setf (smp-ref in-data i) (funcall input 1)))))))
      ;; Create current grain.
      (setf scl +sample-zero+)
      (loop with cur-start = (random s20 random-state)
            with lim = (let ((new (- inlen cur-start)))
                         (if (> grain-len new)
                             (prog1 new (clear-floats grain (- grain-len new)))
                             grain-len))
            for i below lim
            for j from cur-start
            with sus-start = (min lim ramp)
            with sus-end = (min lim (- grain-len ramp)) do
              (setf (aref grain i) (* scl (smp-ref in-data j)))
              (cond ((< i sus-start) (incf scl step))
                    ((>= i sus-end) (decf scl step))))
      ;; Add new grain into output buffer.
      (loop with len = (min outlen (if edit (funcall edit self) grain-len))
            for i below (if (plusp len) len grain-len) do
              (incf (smp-ref out-data i) (aref grain i)))
      ;; Set location of next grain calculation.
      (setf cur-out (max 0 (- (+ output-hop (random (* 2 s50) random-state))
                              half-s50)))
      (if first-samp-p
          (setf first-samp-p nil
                result (smp-ref out-data 0)
                ctr 1)
          (setf ctr 0)))
    result))

(defun* make-granulate (input (expansion 1.0) (length .15) (scaler .6) (hop .05)
                        (ramp .4) (jitter 1.0) max-size edit)
  (unless (plusp expansion)
    (error 'cudere-clm-error
           :format-control "expansion must be > 0.0: ~A"
           :format-arguments (list expansion)))
  (let ((outlen (max (or max-size 0)
                     (floor (* *srate* (+ hop length))))))
    (unless (plusp outlen)
      (error 'cudere-clm-error
             :format-control "size is ~D (hop: ~A, segment-length: ~A)?"
             :format-arguments (list outlen hop length)))
    (when (< (* hop *srate*) expansion)
      (error 'cudere-clm-error
             :format-control "expansion (~A) must be < hop * srate (~A)"
             :format-arguments (list expansion (* hop *srate*))))
    (let* ((grain-len (ceiling (* length *srate*)))
           (rd (cond ((mus-input? input) input)
                     ((readin? input) (readin-sf input))
                     ((and (or (stringp input) (pathnamep input))
                           (dynamic-incudine-finalizer-p))
                      (make-file->sample (mus-file-name input)))))
           (input (cond ((functionp input) input)
                        ((mus-input? rd)
                         (lambda (x)
                           (declare (ignore x))
                           (soundfile:read-next rd)))
                        (t
                         (constantly 0d0))))
           (u (funcall
                (cudere-clm.ugens:granulate
                  hop scaler expansion jitter (floor (* ramp grain-len)) outlen
                  (random (ash 1 32)) input edit (make-double-array grain-len)
                  grain-len 0 nil))))
      (set-granulate-self u u))))

(declaim (inline granulate))
(defun granulate (e &optional input-function edit-function)
  (when (and input-function (not (eq input-function (granulate-input e))))
    (set-granulate-input e input-function))
  (when (and edit-function (not (eq edit-function (granulate-edit e))))
    (set-granulate-edit e edit-function))
  (ugen-tick e))

(silence-compiler-if-no-args granulate (e) `(ugen-tick ,e))

(defmethod mus-reset ((gen granulate-instance))
  (funcall (ugen-reinit-function gen) (mus-frequency gen) (mus-scaler gen)
           (mus-increment gen) (mus-offset gen) (mus-ramp gen)
           (granulate-max-size gen) (mus-location gen) (granulate-input gen)
           (granulate-edit gen) (mus-data gen) (mus-length gen) (mus-hop gen)
           (granulate-self gen))
  gen)

;;; PHASE-VOCODER

(define-clm-ugen phase-vocoder sample
    ((input function) (fft-size positive-fixnum) (hop positive-fixnum)
     (interp positive-fixnum) pitch
     (freqs (simple-array double-float (*)))
     (amps (simple-array double-float (*)))
     (phases (simple-array double-float (*)))
     (amp-increments (simple-array double-float (*)))
     (phase-increments (simple-array double-float (*)))
     (analyze (or function null)) (edit (or function null))
     (synthesize (or function null))
     (outctr non-negative-fixnum) (fft-instance fft)
     (self (or ugen-instance null)))
  (:instance-type phase-vocoder-instance)
  (:readers (fft-size :name mus-length :arg-name gen :method-p t)
            (amps :arg-name gen) (freqs :arg-name gen) (phases :arg-name gen)
            (amp-increments :arg-name gen) (phase-increments :arg-name gen)
            (analyze :arg-name gen) (edit :arg-name gen) (synthesize :arg-name gen)
            (outctr :name mus-location :arg-name gen :method-p t)
            (fft-instance :arg-name gen))
  (:accessors (input :arg-name gen)
              (hop :name mus-hop :arg-name gen :method-p t)
              (interp :name mus-increment :arg-name gen :method-p t)
              (pitch :name mus-frequency :arg-name gen :method-p t)
              (self :arg-name gen))
  (with ((n2 (length phases))
         (filptr 0)
         (last-phases (make-frame n2 :zero-p t))
         (win (make-frame fft-size))
         (in-data (make-frame fft-size))
         (scl (/ (sample interp)))
         (ascl (/ (/ 2d0 0.54) fft-size))
         (pscl (/ (sample hop)))
         (kscl (/ +twopi+ fft-size))
         (ks 0)
         (diff 0)
         (sum 0)
         (sum1 0)
         (calc-p t)
         (first-samp-p t))
    (declare (type non-negative-fixnum n2 filptr)
             (type frame last-phases win in-data)
             (type sample scl ascl pscl kscl ks diff sum sum1)
             (type boolean calc-p first-samp-p))
    (initialize
      (setf outctr interp)
      (dotimes (i fft-size)
        (setf (smp-ref in-data i) (funcall input 1)))
      (funcall (gen:hamming) win fft-size))
    (when (>= outctr interp)
      (setf outctr 0)
      ;; Analysis.
      (when (or (null analyze) (funcall analyze self input))
        (clear-floats freqs fft-size)
        (if first-samp-p
            (setf first-samp-p nil)
            (loop for i below fft-size
                  for j from hop do
                    (setf (smp-ref in-data i)
                          (if (< j fft-size)
                              (smp-ref in-data j)
                              (funcall input 1)))))
        (loop for i below fft-size
              for buf = (mod filptr fft-size) then (mod (1+ buf) fft-size) do
                (setf (aref amp-increments buf)
                      (* (smp-ref win i) ascl (smp-ref in-data i))))
        (incf filptr hop)
        (fft amp-increments freqs fft-size 1 fft-instance)
        (rectangular->polar amp-increments freqs))
      ;; Editing.
      (when (or (null edit) (funcall edit self))
        (setf ks 0d0)
        (dotimes (i n2)
          (setf diff (- (aref freqs i) (smp-ref last-phases i)))
          (setf (smp-ref last-phases i) (aref freqs i))
          (loop while (> diff pi) do (decf diff +twopi+))
          (loop while (< diff (- pi)) do (incf diff +twopi+))
          (setf (aref freqs i) (* pitch (+ (* diff pscl) ks)))
          (incf ks kscl)))
      (dotimes (i n2)
        (setf (aref amp-increments i)
              (* scl (- (aref amp-increments i) (aref amps i))))
        (setf (aref freqs i)
              (* scl (- (aref freqs i) (aref phase-increments i))))))
    (incf outctr)
    ;; Resynthesis.
    (cond (synthesize
           (funcall synthesize self))
          (calc-p
           (setf sum 0d0 sum1 0d0)
           (loop for i below n2 do
                   (incf (aref phase-increments i) (aref freqs i))
                   (incf (aref phases i) (aref phase-increments i))
                   (incf (aref amps i) (aref amp-increments i))
                   (when (plusp (aref amps i))
                     (incf sum (* (aref amps i) (sin (aref phases i)))))
                   (incf (aref phase-increments i) (aref freqs i))
                   (incf (aref phases i) (aref phase-increments i))
                   (incf (aref amps i) (aref amp-increments i))
                   (when (plusp (aref amps i))
                     (incf sum1 (* (aref amps i) (sin (aref phases i))))))
           (setf calc-p nil)
           sum)
          (t
           (setf calc-p t)
           sum1))))

(defun* make-phase-vocoder (input (fft-size 512) (overlap 4) (interp 128)
                            (pitch 1.0) analyze edit synthesize
                            (fft-instance *fft*))
  (declare (type positive-fixnum fft-size overlap interp) (type real pitch))
  (let* ((n2 (ash fft-size -1))
         (rd (cond ((mus-input? input) input)
                   ((readin? input) (readin-sf input))
                   ((and (or (stringp input) (pathnamep input))
                         (dynamic-incudine-finalizer-p))
                    (make-file->sample (mus-file-name input)))))
         (input (cond ((functionp input) input)
                      ((mus-input? rd)
                       (lambda (x)
                         (declare (ignore x))
                         (soundfile:read-next rd)))
                      (t
                       (constantly 0d0))))
         (u (funcall
              (cudere-clm.ugens:phase-vocoder
                input fft-size (floor (/ fft-size overlap)) interp pitch
                (make-double-array fft-size :initial-element 0d0) ; freqs
                (make-double-array n2 :initial-element 0d0)       ; amps
                (make-double-array n2 :initial-element 0d0)       ; phases
                (make-double-array fft-size :initial-element 0d0) ; amp-increments
                (make-double-array n2 :initial-element 0d0)       ; phase-increments
                analyze edit synthesize 0 fft-instance nil))))
    (set-phase-vocoder-self u u)))

(declaim (inline phase-vocoder))
(defun phase-vocoder (pv &optional input)
  (when (and input (not (eq input (phase-vocoder-input pv))))
    (set-phase-vocoder-input pv input))
  (ugen-tick pv))

(silence-compiler-if-no-args phase-vocoder (pv) `(ugen-tick ,pv))

(defmethod mus-reset ((gen phase-vocoder-instance))
  (funcall (ugen-reinit-function gen) (phase-vocoder-input gen)
           (mus-length gen) (mus-hop gen) (mus-increment gen)
           (mus-frequency gen) (phase-vocoder-freqs gen)
           (phase-vocoder-amps gen) (phase-vocoder-phases gen)
           (phase-vocoder-amp-increments gen)
           (phase-vocoder-phase-increments gen)
           (phase-vocoder-analyze gen) (phase-vocoder-edit gen)
           (phase-vocoder-synthesize gen)
           (mus-increment gen) (phase-vocoder-fft-instance gen)
           (phase-vocoder-self gen))
  gen)
