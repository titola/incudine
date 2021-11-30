;;; Copyright (c) 2013-2021 Tito Latini
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

(in-package :incudine)

(defstruct (tempo (:include incudine-object)
                  (:constructor %make-tempo)
                  (:copier nil))
  "Tempo type."
  (ptr (null-pointer) :type foreign-pointer)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(setf (documentation 'tempo-p 'function) "Return T if object is of type TEMPO.")

(define-constant +tempo-pool-initial-size+ 20)

(defvar *tempo-pool*
  (make-incudine-object-pool +tempo-pool-initial-size+ #'%make-tempo nil))
(declaim (type incudine-object-pool *tempo-pool*))

(defvar *rt-tempo-pool*
  (make-incudine-object-pool +tempo-pool-initial-size+ #'%make-tempo t))
(declaim (type incudine-object-pool *rt-tempo-pool*))

(defun make-tempo (value &optional (unit :bpm))
  (declare (type alexandria:positive-real value)
           (type (member :bpm :spb :bps) unit))
  "Create and return a new TEMPO structure.

UNIT is :BPM (default), :SPB or :BPS to set the tempo VALUE in beats
per minute, seconds per beat or beats per second respectively."
  (let ((rt-p (allow-rt-memory-p)))
    (multiple-value-bind (data obj free-fn pool)
        (if rt-p
            (values (foreign-rt-alloc 'sample :count 3)
                    (incudine.util::alloc-rt-object *rt-tempo-pool*)
                    #'safe-foreign-rt-free
                    *rt-tempo-pool*)
            (values (foreign-alloc 'sample :count 3)
                    (incudine.util::alloc-object *tempo-pool*)
                    #'foreign-free
                    *tempo-pool*))
      (declare (type foreign-pointer data) (type tempo obj))
      (incudine-finalize obj
        (lambda ()
          (funcall free-fn data)
          (incudine-object-pool-expand pool 1)))
      (symbol-macrolet ((a (smp-ref data 0))
                        (b (smp-ref data 1))
                        (c (smp-ref data 2)))
        (let ((x (reduce-warnings (sample value))))
          (case unit
            (:bpm (setf a x b (/ 60 x) c (* x #.(/ (sample 60)))))
            (:spb (setf a (/ 60 x) b x c (/ x)))
            (:bps (setf a (* 60 x) b (/ x) c x)))))
      (setf (tempo-ptr obj) data
            (tempo-real-time-p obj) rt-p
            (tempo-foreign-free obj) free-fn)
      obj)))

(defmethod free-p ((obj tempo))
  (null-pointer-p (tempo-ptr obj)))

(defmethod free ((obj tempo))
  (unless (free-p obj)
    (funcall (tempo-foreign-free obj) (tempo-ptr obj))
    (incudine-cancel-finalization obj)
    (setf (tempo-ptr obj) (null-pointer))
    (if (tempo-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-tempo-pool*)
        (incudine.util::free-object obj *tempo-pool*))
    (nrt-msg debug "Free ~A" (type-of obj))
    (values)))

;;; Default tempo
(defvar *tempo* (make-tempo *default-bpm*)
  "Default TEMPO structure.

The value is initially *DEFAULT-BPM* beats per minute.")
(declaim (type tempo *tempo*))

(declaim (inline bpm))
(defun bpm (tempo)
  "Return the tempo in beats per minute. Setfable."
  (smp-ref (tempo-ptr tempo) 0))

(defun set-bpm (tempo bpm)
  (setf #1=(smp-ref (tempo-ptr tempo) 0) (sample bpm))
  (setf (smp-ref (tempo-ptr tempo) 1) (/ 60 #1#))
  (setf (smp-ref (tempo-ptr tempo) 2) (* #1# #.(/ (sample 60))))
  bpm)

(defsetf bpm set-bpm)

(declaim (inline spb))
(defun spb (tempo)
  "Return the tempo in seconds per beat. Setfable."
  (smp-ref (tempo-ptr tempo) 1))

(defun set-spb (tempo spb)
  (setf #1=(smp-ref (tempo-ptr tempo) 1) (sample spb))
  (setf (smp-ref (tempo-ptr tempo) 0) (/ 60 #1#))
  (setf (smp-ref (tempo-ptr tempo) 2) (/ #1#))
  spb)

(defsetf spb set-spb)

(declaim (inline bps))
(defun bps (tempo)
  "Return the tempo in beats per second. Setfable."
  (smp-ref (tempo-ptr tempo) 2))

(defun set-bps (tempo bps)
  (setf #1=(smp-ref (tempo-ptr tempo) 2) (sample bps))
  (setf (smp-ref (tempo-ptr tempo) 0) (* #1# 60))
  (setf (smp-ref (tempo-ptr tempo) 1) (/ #1#))
  bps)

(defsetf bps set-bps)

(defmethod print-object ((obj tempo) stream)
  (print-unreadable-object (obj stream)
    (format stream "TEMPO ~,2F" (if (free-p obj) .0 (bpm obj)))))

(declaim (inline incf-sample-counter))
(defun incf-sample-counter (&optional (delta 1))
  (incf (smp-ref *sample-counter* 0) delta)
  (values))

(declaim (inline reset-timer))
(defun reset-sample-counter ()
  (setf (smp-ref *sample-counter* 0) +sample-zero+)
  (values))

(declaim (inline tempo-sync))
(defun tempo-sync (period)
  "Get the time synchronized to PERIOD samples. It is equivalent to

    (- (+ (now) period) (mod (now) period))"
  (incudine.external::%tempo-sync *sample-counter* (sample period)))

(defvar *dummy-envelope* (%make-envelope))
(declaim (type envelope *dummy-envelope*))

(defstruct (tempo-envelope (:include incudine-object)
                           (:constructor %make-tempo-envelope)
                           (:copier nil))
  "Temporal envelope type."
  (spb *dummy-envelope* :type envelope)
  (cached-seconds (null-pointer) :type foreign-pointer)
  (cached-beats (null-pointer) :type foreign-pointer)
  (points 0 :type non-negative-fixnum)
  (max-points *envelope-default-max-points* :type non-negative-fixnum)
  (constant-p t :type boolean))

(setf (documentation 'tempo-envelope-p 'function)
      "Return T if object is of type TEMPO-ENVELOPE.")

(define-constant +tempo-envelope-pool-initial-size+ 50)

(defvar *tempo-envelope-pool*
  (make-incudine-object-pool +tempo-envelope-pool-initial-size+
                             #'%make-tempo-envelope nil))
(declaim (type incudine-object-pool *tempo-envelope-pool*))

(defvar *rt-tempo-envelope-pool*
  (make-incudine-object-pool +tempo-envelope-pool-initial-size+
                             #'%make-tempo-envelope t))
(declaim (type incudine-object-pool *rt-tempo-envelope-pool*))

(defmethod print-object ((obj tempo-envelope) stream)
  (let ((spb-env (tempo-envelope-spb obj)))
    (print-unreadable-object (obj stream :type t)
      (format stream ":POINTS ~D :LOOP-NODE ~D :RELEASE-NODE ~D"
              (tempo-envelope-points obj) (envelope-loop-node spb-env)
              (envelope-release-node spb-env)))))

(declaim (inline tenv-constant-p))
(defun tenv-constant-p (values)
  (apply #'= values))

(defun make-tempo-envelope (bpms beats &key curve (loop-node -1)
                            (release-node -1) restart-level
                            (real-time-p (allow-rt-memory-p)))
  "Create and return a new TEMPO-ENVELOPE structure from a list of
tempo values in beats per minute and a list of times in beats.

CURVE sets the shape of the segments; the possible values are :STEP,
:LIN or :LINEAR (default), :EXP or :EXPONENTIAL, :SIN or :SINE, :WEL
or :WELCH, :SQR or :SQUARE, :CUB or :CUBIC, a number that represents
the curvature value between two levels for all the segments or a list
of the prior values to specify the curvature values for each segment.
CURVE is ignored if BASE is non-NIL.

If the envelope is sustained, RELEASE-NODE specifies the point of the
release (starting from 0). The default is -1 that means 'envelope
without sustain'.

If LOOP-NODE is a non-negative value, it is the starting point of the
loop of the segments during the sustain phase of the envelope. The
ending point is the point that precedes the release point
RELEASE-NODE.

If RESTART-LEVEL is NIL (default), the envelope restarts from the
current level otherwise it restarts from the value RESTART-LEVEL.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type list bpms beats) (type fixnum loop-node release-node)
           (type boolean real-time-p))
  (let* ((spbs (mapcar (lambda (bpm) (/ (sample 60) bpm)) bpms))
         (rt-p (and real-time-p *allow-rt-memory-pool-p*))
         (spb-env (make-envelope spbs beats :curve curve :loop-node loop-node
                                 :release-node release-node
                                 :restart-level restart-level
                                 :real-time-p rt-p))
         (%points (envelope-points spb-env))
         (%max-points (max %points *envelope-default-max-points*))
         (cached-data-size (* %max-points 2)))
    (declare #.*reduce-warnings*)
    (multiple-value-bind (cached-data tempo-env free-fn pool)
        (if rt-p
            (values (foreign-rt-alloc 'sample :count cached-data-size)
                    (incudine.util::alloc-rt-object *rt-tempo-envelope-pool*)
                    #'safe-foreign-rt-free
                    *rt-tempo-envelope-pool*)
            (values (foreign-alloc-sample cached-data-size)
                    (incudine.util::alloc-object *tempo-envelope-pool*)
                    #'foreign-free
                    *tempo-envelope-pool*))
      (handler-case
          (incudine.util::with-struct-slots
              ((spb cached-seconds cached-beats points
                max-points constant-p) tempo-env tempo-envelope)
            (setf spb spb-env
                  cached-seconds cached-data
                  cached-beats (cffi:mem-aptr cached-data 'sample %max-points)
                  points %points
                  max-points %max-points
                  constant-p (tenv-constant-p spbs))
            (fill-cached-data-seconds cached-data spb-env)
            (fill-cached-data-beats cached-beats spb-env))
        (condition (c)
          (funcall free-fn cached-data)
          (free spb-env)
          (incudine-object-pool-expand pool 1)
          (error c)))
      (incudine-finalize tempo-env
        (lambda ()
          (funcall free-fn cached-data)
          (incudine-object-pool-expand pool 1))))))

(defmethod free-p ((obj tempo-envelope))
  (null-pointer-p (tempo-envelope-cached-seconds obj)))

(defmethod free ((obj tempo-envelope))
  (unless (free-p obj)
    (let ((env (tempo-envelope-spb obj)))
      (declare (type envelope env))
      ;; Free also the memory space for cached-beats.
      (funcall (envelope-foreign-free env) (tempo-envelope-cached-seconds obj))
      (incudine-cancel-finalization obj)
      (setf (tempo-envelope-cached-seconds obj) (null-pointer))
      (setf (tempo-envelope-cached-beats obj) (null-pointer))
      (free env)
      (setf (tempo-envelope-spb obj) *dummy-envelope*)
      (setf (tempo-envelope-points obj) 0)
      (if (envelope-real-time-p env)
          (incudine.util::free-rt-object obj *rt-tempo-envelope-pool*)
          (incudine.util::free-object obj *tempo-envelope-pool*))
      (nrt-msg debug "Free ~A" (type-of obj))))
  (values))

(defun copy-tempo-envelope (tenv)
  "Return a copy of a TEMPO-ENVELOPE structure."
  (declare (type tempo-envelope tenv))
  (if (free-p tenv)
      (incudine-error "The temporal envelope is unusable.")
      (let* ((%points (tempo-envelope-points tenv))
             (%max-points (tempo-envelope-max-points tenv))
             (cached-data-size (* %max-points 2))
             (rt-p (allow-rt-memory-p)))
        (multiple-value-bind (twarp-data new free-fn pool)
            (reduce-warnings
              (if rt-p
                  (values (foreign-rt-alloc 'sample :count cached-data-size)
                          (incudine.util::alloc-rt-object *rt-tempo-envelope-pool*)
                          #'safe-foreign-rt-free
                          *rt-tempo-envelope-pool*)
                  (values (foreign-alloc-sample cached-data-size)
                          (incudine.util::alloc-object *tempo-envelope-pool*)
                          #'foreign-free
                          *tempo-envelope-pool*)))
          (incudine.util::with-struct-slots
              ((spb cached-seconds points max-points constant-p)
               new tempo-envelope)
            (setf spb (copy-envelope (tempo-envelope-spb tenv))
                  cached-seconds twarp-data
                  points %points
                  max-points %max-points
                  constant-p (tempo-envelope-constant-p tenv))
            (incudine-finalize new
              (lambda ()
                (funcall free-fn twarp-data)
                (incudine-object-pool-expand pool 1)))
            (foreign-copy-samples
              twarp-data (tempo-envelope-cached-seconds tenv) cached-data-size)
            (setf (tempo-envelope-cached-beats new)
                  (cffi:mem-aptr twarp-data 'sample %max-points))
            new)))))

;;; Integrated curves to get BEATS->SECONDS.
;;;
;;; The minimal duration in beats is one sample. The duration in
;;; seconds of a segment with arbitrary shape is the sum of the
;;; seconds-per-beat (SPB) for each sample:
;;;
;;;     sample 0, SPB0  -->  t0 + 0 beats = SPB0 seconds
;;;     sample 1, SPB1  -->  t0 + 1/SR beats = SPB0 + SPB1 seconds
;;;     sample 2, SPB2  -->  t0 + 2/SR beats = SPB0 + SPB1 + SPB2 seconds
;;;     [...]
;;;     sample N, SPBN  -->  t0 + N/SR beats = SPB0 + ... + SPBN seconds
;;;
;;; where SR is the sampling rate and t0 is the time in beats at the
;;; beginning of the curvature.
;;;
;;; In the context of BEATS->SECONDS, x is the normalized time in
;;; beats between 0 and 1, k0 and k1 are seconds-per-beat.
;;;
;;; Linear curve: (k1 - k0) x + k0,   x in [0;1]
(declaim (inline integrate-linear-curve))
(defun integrate-linear-curve (k0 k1 x)
  (let ((x2 (* x x)))
    (* 0.5 (+ (* x2 k1) (* (- (* 2 x) x2) k0)))))

;;; Exponential curve: k0 (k1 / k0)^x,   x in [0;1]
(declaim (inline integrate-exp-curve))
(defun integrate-exp-curve (k0 k1 x)
  (let* ((c0 (/ k1 k0))
         (c1 (log c0)))
    (* k0 (- (/ (expt c0 x) c1) (/ 1.0 c1)))))

;;; Sinusoidal curve: 0.5 (k1 - k0) (1 - cos(pi x)) + k0,   x in [0;1]
(declaim (inline integrate-sine-curve))
(defun integrate-sine-curve (k0 k1 x)
  (/ (- (+ (* (- k1 k0) (sin (* pi x)))
           (* (- (* (- pi) k1) (* pi k0)) x)))
     +twopi+))

;;; Welch curve:
;;;   if k1 >= k0, k0 + (k1 - k0) sin(0.5 pi x)
;;;   if k1 < k0,  k1 + (k0 - k1) cos(0.5 pi x)
;;;   x in [0;1]
(declaim (inline integrate-welch-curve))
(defun integrate-welch-curve (k0 k1 x)
  (let* ((c0 (* pi x))
         (c1 (* 0.5 c0)))
    (/ (if (>= k1 k0)
           (let ((c2 (* 2 (cos c1))))
             (+ (* (- c2 2) k1) (* (- 2 c0 c2) k0)))
           (let ((c2 (* 2 (sin c1))))
             (- (* (- c2 c0) k1) (* c2 k0))))
       (- pi))))

;;; Square curve: (sqrt(k0) + (sqrt(k1) - sqrt(k0)) x)^2,   x in [0;1]
(declaim (inline integrate-square-curve))
(defun integrate-square-curve (k0 k1 x)
  (let* ((x2 (* x x))
         (x3 (* x2 x)))
    (/ (+ (* (- x3) k1) (* (- (* 2 x3) (* 3 x2)) (sqrt k0) (sqrt k1))
          (* (- (+ (- x3) (* 3 x2)) (* 3 x)) k0))
       -3.0)))

;;; Cubic curve: (a + (b - a)*x)^3,   x in [0;1]
;;;       1/3        1/3
;;; a = k0   , b = k1
(declaim (inline integrate-cubic-curve))
(defun integrate-cubic-curve (k0 k1 x)
  (let* ((x2 (* x x))
         (x3 (* x2 x))
         (x4 (* x2 x2))
         (c0 (expt k0 1/3))
         (c1 (expt k1 1/3)))
    (* -0.25 (+ (* (- x4) k1) (* (- (* 3 x4) (* 4 x3)) c0 c1 c1)
                (* (+ (* -3 x4) (* 8 x3) (* -6 x2)) (* c0 c0 c1))
                (* (+ x4 (* -4 x3) (* 6 x2) (* -4 x)) k0)))))

;;; Custom curve:
;;;
;;;       k1 - k0
;;; k0 + ---------- (1 - exp(c x))
;;;      1 - exp(c)
;;;
(declaim (inline integrate-custom-curve))
(defun integrate-custom-curve (k0 k1 curve x)
  (let* ((c0 (exp curve))
         (c1 (- (* curve c0) curve))
         (cx (* curve x))
         (c2 (exp cx)))
    (/ (+ (* (- c2 cx) k1) (* (- (* cx c0) c2) k0) (- k0 k1)) c1)))

(defun segment-time-seconds (spb0 spb1 curve duration time)
  (* duration
     (if (or (= spb0 spb1) (= curve +seg-step-func+))
         (* spb0 time)
         (curve-case curve
           (+seg-lin-func+ (integrate-linear-curve spb0 spb1 time))
           (+seg-exp-func+ (integrate-exp-curve spb0 spb1 time))
           (+seg-sine-func+ (integrate-sine-curve spb0 spb1 time))
           (+seg-welch-func+ (integrate-welch-curve spb0 spb1 time))
           (+seg-square-func+ (integrate-square-curve spb0 spb1 time))
           (+seg-cubic-func+ (integrate-cubic-curve spb0 spb1 time))
           (otherwise (integrate-custom-curve spb0 spb1 curve time))))))

;;; Resolving the integrated curves to get the time in beats from
;;; SECONDS->BEATS, the reciprocal of BEATS->SECONDS.
;;;
;;; From the integrated linear curve:
;;;
;;;    2     2 k0        2 y
;;;   x  + ------- x - ------- = 0
;;;        k1 - k0     k1 - k0
;;;
;;;                              2
;;;       sqrt(2 y (k1 - k0) + k0 ) - k0
;;;   x = ------------------------------;    x in [0;1], y in [0;(k0+k1)/2]
;;;                 k1 - k0
;;;
;;; In the context of SECONDS->BEATS, x and y are the unscaled times in
;;; beats and seconds respectively, k0 and k1 are seconds-per-beat.
;;; The value in seconds is
;;;
;;;     curve_duration * f(y=pos/curve_duration),
;;;
;;; with pos between 0 and curve_duration.
;;;
;;; The normalized values simplify the solution of the equation.
;;;
(declaim (inline inverse-integrate-linear-curve))
(defun inverse-integrate-linear-curve (k0 k1 y)
  (let ((kdiff (- k1 k0)))
    (/ (- (sqrt (+ (* 2 y kdiff) (* k0 k0))) k0) kdiff)))

;;; From the integrated exponential curve:
;;;
;;;        k1 x
;;;   k0 ((--)  - 1)
;;;        k0
;;;   -------------- - y = 0
;;;          k1
;;;      log(--)
;;;          k0
;;;
;;;
;;;               k1
;;;           log(--) y
;;;               k0
;;;       log(--------- + 1)
;;;              k0
;;;   x = ------------------;    x in [0;1], y in [0;(k1-k0)/log(k1/k0)]
;;;                k1
;;;            log(--)
;;;                k0
;;;
(declaim (inline inverse-integrate-exp-curve))
(defun inverse-integrate-exp-curve (k0 k1 y)
  (let ((c (log (/ k1 k0))))
    (/ (log (1+ (/ (* c y) k0))) c)))

;;; From the integrated sinusoidal curve:
;;;
;;;   (k1 - k0) sin(pi x) - pi (k0 + k1) x
;;;   ------------------------------------ + y = 0
;;;                   2 pi
;;;
;;; approximated by
;;;
;;;                           ((pi - 2) k1 + (pi + 2) k0) x (x - 1)
;;;   (k1 + k0) x (x - 0.5) - ------------------------------------- = 0
;;;                                            pi
;;;
;;;       sqrt(4 pi a y + b^2) - b
;;;   x = ------------------------;    x in [0;1], y in [0;(k0+k1)/2]
;;;                  a
;;;
;;;   a = 8 (k1 - k0)
;;;   b = (pi + 4) k0 + (pi - 4) k1
;;;
(declaim (inline inverse-integrate-sine-curve))
(defun inverse-integrate-sine-curve (k0 k1 y)
  (let ((a (* 8 (- k1 k0)))
        (b (+ (* (+ pi 4) k0) (* (- pi 4) k1))))
    (/ (- (sqrt (+ (* 4 pi a y) (* b b))) b) a)))

;;; The integrated Welch curve is approximated by
;;;
;;; [k1 >= k0]
;;;
;;;   2 (2 (k1 - k0) + pi k0) (x - 0.5) x
;;;   ----------------------------------- +
;;;                  pi
;;;
;;;     4 (2 (k1 - k0) + sqrt(2) (k0 - k1) + 0.5 pi k0) (x - 1) x
;;;   - --------------------------------------------------------- - y = 0
;;;                              pi
;;;
;;;       sqrt(2 pi c y + b^2) - b
;;;   x = ------------------------;    x in [0;1], y in [0;(pi*k0+2(k1-k0))/pi]
;;;                 c
;;;
;;;        7/2
;;;   a = 2   - 8
;;;   kdelta = k1 - k0
;;;   b = 2 (a - pi) kdelta + pi k0
;;;   c = a kdelta
;;;
;;; [k1 < k0]
;;;
;;;   2 (pi k1 - 2 (k1 - k0)) (x - 0.5) x
;;;   ----------------------------------- +
;;;                   pi
;;;
;;;     4 (0.5 pi k1 + sqrt(2) (k0 - k1)) (x - 1) x
;;;   - ------------------------------------------- - y = 0
;;;                         pi
;;;
;;;       sqrt(2 pi c y + b^2) + b
;;;   x = ------------------------;    x in [0;1], y in [0;(pi*k1-2(k1-k0))/pi]
;;;                  c
;;;
;;;        7/2
;;;   a = 2   - 8
;;;   kdelta = k1 - k0
;;;   b = 3 (a - pi) kdelta - pi k0
;;;   c = a kdelta
;;;
(defun inverse-integrate-welch-curve (k0 k1 y)
  (let* ((a (- (expt 2d0 7/2) 8))
         (kdelta (- k1 k0))
         (c (* a kdelta))
         (y2 (* +twopi+ c y))
         (m (* (- a pi) kdelta))
         (n (* pi k0)))
    (if (>= k1 k0)
        (let ((b (+ (* 2 m) n)))
          (/ (- (sqrt (+ y2 (* b b))) b) c))
        (let ((b (- (* 3 m) n)))
          (/ (+ (sqrt (+ y2 (* b b))) b) c)))))

;;; From the integrated square curve:
;;;
;;;   c^2 x^3 + (3 a b - 3 a^2) x^2 + 3 a^2 x - 3 y = 0
;;;
;;;                 3 1/3
;;;       (3 c y + a )   - a
;;;   x = ------------------;    x in [0;1], y in [0;(k0+k1+sqrt(k0)sqrt(k1))/3]
;;;               c
;;;
;;;   a = sqrt(k0)
;;;   b = sqrt(k1)
;;;   c = b - a
;;;
(declaim (inline inverse-integrate-square-curve))
(defun inverse-integrate-square-curve (k0 k1 y)
  (let* ((a (sqrt k0))
         (b (sqrt k1))
         (c (- b a)))
    (/ (- (expt (+ (* 3 c y) (* k0 a)) 1/3) a) c)))

;;; From the integrated cubic curve:
;;;
;;;           3           2      2         3
;;;   x ((c x) + 4 a (c x)  + 6 a c x + 4 a ) - 4 y = 0
;;;
;;;                 4 1/4
;;;       (4 c y + a )   - a
;;;   x = ------------------;   x in [0;1], y in [0;(2(k0+k1)+(a+b)^3)/12]
;;;               c
;;;
;;;         1/3        1/3
;;;   a = k0   , b = k1   , c = b - a
;;;
(declaim (inline inverse-integrate-cubic-curve))
(defun inverse-integrate-cubic-curve (k0 k1 y)
  (let* ((a (expt k0 1/3))
         (b (expt k1 1/3))
         (c (- b a)))
    (/ (- (expt (+ (* 4 c y) (expt a 4)) 1/4) a) c)))

;;; The solution of the integrated custom curve is approximated by
;;;
;;;                    3                         2 1/2
;;;       (8 c kdelta n (m + 1) y + (a k1 - b k0) )   + a k1 - b k0
;;;   x = ---------------------------------------------------------
;;;                                       2
;;;                             4 kdelta n
;;;
;;;   x in [0;1], y in [0;(m^2 (k1-k0)-c(k1-m^2 k0)-k1+k0)/(c*(m^2-1))]
;;;
;;;   kdelta = k1 - k0
;;;   a = m^2 - 4 m + c + 3
;;;   b = (c+1)*m^2 - 4 m + 3
;;;   m = e^(c/2)
;;;   n = m - 1
;;;
(defun inverse-integrate-custom-curve (k0 k1 curve y)
  (let* ((m (exp (* 0.5 curve)))
         (m2 (* m m))
         (n (- m 1))
         (n2 (* n n))
         (q (- 3 (* 4 m)))
         (a (+ m2 q curve))
         (b (+ (* (+ curve 1) m2) q))
         (kdelta (- k1 k0))
         (kdelta2 (* 4 kdelta n2))
         (kdelta3 (- (* a k1) (* b k0))))
    (/ (+ (sqrt (+ (* 2 curve kdelta2 n (+ m 1) y)
                   (* kdelta3 kdelta3)))
          kdelta3)
       kdelta2)))

(defun segment-time-beats (spb0 spb1 curve duration time)
  (* duration
     (if (or (= spb0 spb1) (= curve +seg-step-func+))
         (/ time spb0)
         (curve-case curve
           (+seg-lin-func+ (inverse-integrate-linear-curve spb0 spb1 time))
           (+seg-exp-func+ (inverse-integrate-exp-curve spb0 spb1 time))
           (+seg-sine-func+ (inverse-integrate-sine-curve spb0 spb1 time))
           (+seg-welch-func+ (inverse-integrate-welch-curve spb0 spb1 time))
           (+seg-square-func+ (inverse-integrate-square-curve spb0 spb1 time))
           (+seg-cubic-func+ (inverse-integrate-cubic-curve spb0 spb1 time))
           (otherwise (inverse-integrate-custom-curve spb0 spb1 curve time))))))

(defun fill-cached-data-seconds (twarp-data spb-env)
  (let* ((data (envelope-data spb-env))
         (points (envelope-points spb-env))
         (twarp0 (segment-time-seconds (smp-ref data 0) (smp-ref data 2)
                                       (smp-ref data 3) (smp-ref data 1) 1)))
    (setf (smp-ref twarp-data 0) +sample-zero+
          (smp-ref twarp-data 1) twarp0)
    (unless (< points 3)
      (loop for i from 4 to (* (1- points) 3) by 3
            for twdata-index from 2
            for k0 = (smp-ref data 2) then k1
            for k1 = (smp-ref data (+ i 1))
            for t0 = twarp0 then time
            for t1 = (segment-time-seconds k0 k1
                                           (smp-ref data (+ i 2))
                                           (smp-ref data i) 1)
            for time = (+ t0 t1) do
           (setf (smp-ref twarp-data twdata-index) time)))
    (values)))

(defun fill-cached-data-beats (data spb-env)
  (setf (smp-ref data 0) +sample-zero+)
  (loop for i from 1 below (envelope-points spb-env) do
        (setf (smp-ref data i)
              (+ (smp-ref data (1- i))
                 (smp-ref (envelope-data spb-env) (- (* i 3) 2))))))

(defun set-tempo-envelope (env bpms beats &key curve (loop-node -1)
                           (release-node -1) restart-level)
  "Change a TEMPO-ENVELOPE structure.

BPMS is a list of tempo values in beats per minute.

BEATS is a list of times in beats.

CURVE sets the shape of the segments; the possible values are :STEP,
:LIN or :LINEAR (default), :EXP or :EXPONENTIAL, :SIN or :SINE, :WEL
or :WELCH, :SQR or :SQUARE, :CUB or :CUBIC, a number that represents
the curvature value between two levels for all the segments or a list
of the prior values to specify the curvature values for each segment.
CURVE is ignored if BASE is non-NIL.

If the envelope is sustained, RELEASE-NODE specifies the point of the
release (starting from 0). The default is -1 that means 'envelope
without sustain'.

If LOOP-NODE is a non-negative value, it is the starting point of the
loop of the segments during the sustain phase of the envelope. The
ending point is the point that precedes the release point
RELEASE-NODE.

If RESTART-LEVEL is NIL (default), the envelope restarts from the
current level otherwise it restarts from the value RESTART-LEVEL."
  (declare (type tempo-envelope env) (type list bpms beats)
           (type fixnum loop-node release-node))
  (let ((spb-env (tempo-envelope-spb env))
        (cached-data (tempo-envelope-cached-seconds env))
        (cached-data2 (tempo-envelope-cached-beats env))
        (spbs (reduce-warnings
                (mapcar (lambda (bpm) (/ (sample 60) bpm)) bpms))))
    (edit-envelope spb-env spbs beats :curve curve :loop-node loop-node
                   :release-node release-node :restart-level restart-level)
    (setf (tempo-envelope-constant-p env) (tenv-constant-p spbs))
    (let ((points (envelope-points spb-env)))
      (unless (= points (tempo-envelope-points env))
        (when (> points (tempo-envelope-max-points env))
          (let* ((real-time-p (envelope-real-time-p spb-env))
                 (free-function (envelope-foreign-free spb-env))
                 (pool (if real-time-p
                           *rt-tempo-envelope-pool*
                           *tempo-envelope-pool*)))
            (reduce-warnings
              (setf cached-data
                    (if real-time-p
                      (rt-eval (:return-value-p t)
                        (foreign-rt-realloc
                          cached-data 'sample :count (* 2 points)))
                      (foreign-realloc
                        cached-data 'sample :count (* 2 points))))
              (setf cached-data2 (cffi:mem-aptr cached-data 'sample points)))
            (incudine-cancel-finalization env)
            (incudine-finalize env
              (lambda ()
                (funcall free-function cached-data)
                (incudine-object-pool-expand pool 1)))
            (setf (tempo-envelope-cached-seconds env) cached-data)
            (setf (tempo-envelope-cached-beats env) cached-data2)
            (setf (tempo-envelope-max-points env) points)))
        (setf (tempo-envelope-points env) points))
      (fill-cached-data-seconds cached-data spb-env)
      (fill-cached-data-beats cached-data2 spb-env)
      env)))

(defun %beats->seconds (tempo-env beats)
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (if (zerop beats)
      +sample-zero+
      (let* ((last-point-index (1- (tempo-envelope-points tempo-env)))
             (spb-env (tempo-envelope-spb tempo-env))
             (data (envelope-data spb-env))
             (twarp-data (tempo-envelope-cached-seconds tempo-env))
             (pos-time (sample 0))
             (curve-duration (sample 0))
             (t0 (sample 0))
             (t1 (sample 0)))
        (declare (type non-negative-fixnum last-point-index)
                 (type sample pos-time curve-duration t0 t1)
                 (dynamic-extent pos-time curve-duration t0 t1))
        (setf pos-time (sample beats))
        (labels ((look (p index)
                   (declare (type non-negative-fixnum p index)
                            #.*standard-optimize-settings*
                            #.*reduce-warnings*)
                   (setf curve-duration (smp-ref data (- index 2)))
                   (setf t0 t1)
                   (incf t1 curve-duration)
                   (cond ((< pos-time t1)
                          (+ (smp-ref twarp-data (1- p))
                             (segment-time-seconds
                               (smp-ref data (if (= p 1) 0 (- index 4)))
                               (smp-ref data (- index 1))
                               (smp-ref data index)
                               curve-duration
                               (/ (- pos-time t0) curve-duration))))
                         ((< p last-point-index)
                          (look (1+ p) (+ index 3)))
                         (t
                          (let ((last-time (smp-ref twarp-data
                                                    last-point-index)))
                            (+ last-time (* (smp-ref data (- index 1))
                                            (- pos-time t1))))))))
          (look 1 3)))))

(declaim (inline beats->seconds))
(defun beats->seconds (tempo-env beats &optional (offset 0))
  "Convert the time from number of beats to seconds by following a
TEMPO-ENVELOPE structure, starting from OFFSET beats (zero by default)."
  (if (or (zerop offset) (tempo-envelope-constant-p tempo-env))
      (%beats->seconds tempo-env beats)
      (- (%beats->seconds tempo-env (+ offset beats))
         (%beats->seconds tempo-env offset))))

(defun %seconds->beats (tempo-env seconds)
  (declare (type tempo-envelope tempo-env) (type (real 0) seconds))
  (if (zerop seconds)
      +sample-zero+
      (let* ((last-point-index (1- (tempo-envelope-points tempo-env)))
             (spb-env (tempo-envelope-spb tempo-env))
             (data (envelope-data spb-env))
             (cached-beats (tempo-envelope-cached-beats tempo-env))
             (cached-seconds (tempo-envelope-cached-seconds tempo-env))
             (pos-time (sample 0))
             (curve-duration-beats (sample 0))
             (t0 (sample 0))
             (t1 (sample 0)))
        (declare (type non-negative-fixnum last-point-index)
                 (type sample pos-time curve-duration-beats t0 t1)
                 (dynamic-extent pos-time curve-duration-beats t0 t1))
        (setf pos-time (sample seconds))
        (labels ((look (p index)
                   (declare (type non-negative-fixnum p index)
                            #.*standard-optimize-settings*
                            #.*reduce-warnings*)
                   (setf t0 t1)
                   (setf t1 (smp-ref cached-seconds p))
                   (setf curve-duration-beats (smp-ref data (- index 2)))
                   (cond ((< pos-time t1)
                          (+ (smp-ref cached-beats (1- p))
                             (segment-time-beats
                               (smp-ref data (if (= p 1) 0 (- index 4)))
                               (smp-ref data (- index 1))
                               (smp-ref data index)
                               curve-duration-beats
                               (/ (- pos-time t0) curve-duration-beats))))
                         ((< p last-point-index)
                          (look (1+ p) (+ index 3)))
                         (t
                          (let ((last-time (smp-ref cached-beats
                                                    last-point-index)))
                            (+ last-time (/ (- pos-time t1)
                                            (smp-ref data (- index 1)))))))))
          (look 1 3)))))

(declaim (inline seconds->beats))
(defun seconds->beats (tempo-env seconds &optional (offset 0))
  "Convert the time from seconds to number of beats by following a
TEMPO-ENVELOPE structure, starting from OFFSET seconds (zero by default).

The returned time in beats is always precise if the segments of the
envelope have shapes :STEP, :LINEAR, :EXPONENTIAL, :SQUARE or :CUBIC.
It is approximated during the transitions between two BPM's if the
curvatures are sinusoidal, Welch or custom."
  (if (or (zerop offset) (tempo-envelope-constant-p tempo-env))
      (%seconds->beats tempo-env seconds)
      (- (%seconds->beats tempo-env (+ offset seconds))
         (%seconds->beats tempo-env offset))))

(declaim (inline spb-at))
(defun spb-at (tempo-env beats)
  "Return the tempo in seconds per beat of a TEMPO-ENVELOPE structure
at time BEATS."
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (envelope-at (tempo-envelope-spb tempo-env) beats))

(declaim (inline bpm-at))
(defun bpm-at (tempo-env beats)
  "Return the tempo in beats per minute of a TEMPO-ENVELOPE structure
at time BEATS."
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (/ 60.0 (spb-at tempo-env beats)))

(declaim (inline bps-at))
(defun bps-at (tempo-env beats)
  "Return the tempo in beats per second of a TEMPO-ENVELOPE structure
at time BEATS."
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (/ (spb-at tempo-env beats)))

;;; Return the list
;;;     ((0 bpm0) transition-curve (beats bpm1) transition-curve ...)
(defun tempo-map (tempo-env)
  (let* ((spb-env (tempo-envelope-spb tempo-env))
         (points (envelope-points spb-env)))
    (list* (list 0 (/ 60.0 (envelope-level spb-env 0)))
           (unless (zerop (envelope-time spb-env 1))
             (loop for i from 1 below points
                   for beats = (envelope-time spb-env i)
                             then (+ beats (envelope-time spb-env i))
                   append (list (envelope-curve spb-env i)
                                (list beats
                                      (/ 60.0 (envelope-level spb-env i)))))))))

(defun next-tempo-map-bpms (tempo-env curr-list transition-time-step)
  (let ((curve (second curr-list)))
    (if (or (null curve) (eq curve :step))
        (first curr-list)
        (loop for beats from (caar curr-list) below (caaddr curr-list)
                        by transition-time-step
              append (list beats (bpm-at tempo-env beats))))))

(defun tempo-breakpoints (tempo-env &key (transition-time-step 1/8))
  "Return a list of {beats bpm} break-point pairs obtained from a
TEMPO-ENVELOPE structure.

If the curve of a transition is not :STEP, TRANSITION-TIME-STEP is the
time in beats between adjacent points during that transition.

TRANSITION-TIME-STEP defaults to 1/8."
  (loop for lst on (tempo-map tempo-env) by #'cddr
        append (next-tempo-map-bpms tempo-env lst transition-time-step)))

(defmacro case-char (char &body cases)
  (with-gensyms (c)
    `(let ((,c ,char))
       (declare (ignorable ,c))
       (cond ,@(mapcar (lambda (x)
                         `(,(if (eq (car x) 'otherwise)
                                t
                                `(char-equal ,c ,(car x)))
                           ,@(cdr x)))
                       cases)))))

(defun parse-time-unit (string mult arg0 arg1)
  (declare (type simple-string string))
  (case-char (char string 0)
    ;; b.*  -> beats
    (#\b `(* incudine.util:*sample-rate*
             ,(if arg1
                  ;; ARG0 is a TEMPO-ENVELOPE and ARG1 is the
                  ;; start time in beats
                  `(beats->seconds ,arg0 (incudine.util:sample ,mult) ,arg1)
                  ;; ARG0 is an instance of TEMPO
                  `(* (incudine.util:sample ,mult) (spb ,(or arg0 '*tempo*))))))
    ;; s    -> seconds
    ;; se.* -> seconds
    ;; sa.* -> samples
    (#\s `(* ,mult ,(if (> (length string) 1)
                        (case-char (char string 1)
                          (#\e 'incudine.util:*sample-rate*)
                          (#\a (incudine.util:sample 1.0))
                          (otherwise
                           (error 'incudine-unknown-time-unit :name string)))
                        'incudine.util:*sample-rate*)))
    ;; m    -> meters   (the optional ARG0 is the velocity of the sound [m/s])
    ;; me.* -> meters   "            "            "             "            "
    ;; mi.* -> minutes
    ;; ms.* -> msec
    (#\m `(* ,mult ,(if (> (length string) 1)
                        (case-char (char string 1)
                          (#\s '(* 1d-3 incudine.util:*sample-rate*))
                          (#\e `(* ,(if arg0
                                        (/ 1d0 arg0)
                                        'incudine.util:*r-sound-velocity*)
                                   incudine.util:*sample-rate*))
                          (#\i '(* 60d0 incudine.util:*sample-rate*))
                          (otherwise
                           (error 'incudine-unknown-time-unit :name string)))
                        `(* ,(if arg0
                                 (/ 1d0 arg0)
                                 'incudine.util:*r-sound-velocity*)
                            incudine.util:*sample-rate*))))
    ;; h.*  -> hours
    (#\h `(* ,mult 3600d0 incudine.util:*sample-rate*))
    ;; d.*  -> days
    (#\d `(* ,mult 86400d0 incudine.util:*sample-rate*))
    ;; w.*  -> weeks
    (#\w `(* ,mult 604800d0 incudine.util:*sample-rate*))
    (otherwise (error 'incudine-unknown-time-unit :name string))))

(defun parse-time-string (stream subchar arg)
  (declare #.*standard-optimize-settings*
           (type stream stream) (ignore subchar arg))
  (let* ((*read-default-float-format* incudine.config:*sample-type*)
         (l (read-delimited-list #\] stream t)))
    (if l
        (let ((mult (first l)))
          (if (rest l)
              (let ((time-unit-str (symbol-name (second l))))
                (destructuring-bind (&optional tempo beats) (cddr l)
                  (parse-time-unit time-unit-str mult tempo beats)))
              mult))
        +sample-zero+)))

(defun set-sharp-square-bracket-syntax ()
  (set-macro-character #\] (get-macro-character #\) nil))
  (set-dispatch-macro-character #\# #\[ #'parse-time-string))

(defun add-sharp-square-bracket-syntax ()
  (setf *readtable* (copy-readtable *readtable*))
  (set-sharp-square-bracket-syntax))

(pushnew #'add-sharp-square-bracket-syntax *initialize-hook*)

(defmacro enable-sharp-square-bracket-syntax ()
  "Enable the reader syntax #[...] to enter the time in samples by
using different units. The syntax is

    #[number unit]

    #[number-of-beats b.*]

    #[number-of-beats b.* tempo]

    #[number-of-beats b.* tempo-envelope offset-in-beats]

The number of beats depends on a TEMPO or TEMPO-ENVELOPE structure.
The default is *TEMPO*.

The possible units are:

|-------------+------------------------+--------------------------|
| unit        | the symbol starts with | examples                 |
|-------------+------------------------+--------------------------|
| sample      | sa                     | sa, samp, samps, samples |
| millisecond | ms                     | ms, msec                 |
| second      | s                      | s, sec, seconds          |
| minute      | mi                     | mi, min, minutes         |
| hour        | h                      | h, hours                 |
| day         | d                      | d, days                  |
| week        | w                      | w, weeks                 |
| beat        | b                      | b, beats                 |
| meter       | m                      | m, meters                |
|-------------+------------------------+--------------------------|

The number of meters depends on the velocity of the sound in m/s at
22 degrees Celsius, 1 atmosfera (the default is *SOUND-VELOCITY*)."
  `(eval-when (:compile-toplevel :execute)
     (add-sharp-square-bracket-syntax)))

(define-constant unix-to-universal-time 2208988800)

(defun timestamp ()
  "Return a double float value for the current time of day in
universal time format."
  (multiple-value-bind (sec usec) (incudine.util::get-time-of-day)
    (+ (* usec 1d-6) sec unix-to-universal-time)))
