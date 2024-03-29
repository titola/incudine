;;; Copyright (c) 2013-2023 Tito Latini
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

;;; ENVELOPE is inspired by the Env object in SuperCollider

(defvar *envelope-default-max-points* 8)
(declaim (type non-negative-fixnum *envelope-default-max-points*))

;;; Values of the constants used in the past to get an index from 0 to 6
(define-constant +seg-step-func+   (sample  5d15))
(define-constant +seg-lin-func+    (sample  1d16))
(define-constant +seg-exp-func+    (sample  2d16))
(define-constant +seg-sine-func+   (sample  4d16))
(define-constant +seg-welch-func+  (sample  8d16))
(define-constant +seg-square-func+ (sample 16d16))
(define-constant +seg-cubic-func+  (sample 32d16))

(defstruct (envelope (:include incudine-object)
                     (:constructor %make-envelope)
                     (:copier nil))
  "Envelope type."
  (data-ptr (null-pointer) :type foreign-pointer)
  (duration +sample-zero+ :type sample)
  (points 0 :type non-negative-fixnum)
  (data-size 0 :type non-negative-fixnum)
  (loop-node -1 :type fixnum)
  (release-node -1 :type fixnum)
  ;; The envelope restarts from the current level if RESTART-LEVEL is NIL
  (%restart-level nil :type (or sample null))
  (max-points *envelope-default-max-points* :type non-negative-fixnum)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(setf
  (documentation 'envelope-p 'function)
  "Return T if object is of type ENVELOPE."
  (documentation 'envelope-duration 'function)
  "Return the duration of the envelope."
  (documentation 'envelope-points 'function)
  "Return the number of envelope points."
  (documentation 'envelope-loop-node 'function)
  "Return a non-negative value if it is the starting point of the
loop of the segments during the sustain phase of the envelope."
  (documentation 'envelope-release-node 'function)
  "Return the point of the release (starting from 0) or -1 if the
envelope is without sustain.")

(define-constant +envelope-pool-initial-size+ 4000)

(defvar *envelope-pool*
  (make-incudine-object-pool +envelope-pool-initial-size+ #'%make-envelope nil))
(declaim (type incudine-object-pool *envelope-pool*))

(defvar *rt-envelope-pool*
  (make-incudine-object-pool +envelope-pool-initial-size+ #'%make-envelope t))
(declaim (type incudine-object-pool *rt-envelope-pool*))

(declaim (inline envelope-data))
(defun envelope-data (obj)
  "Return the foreign pointer to the envelope data."
  (envelope-data-ptr obj))

(defmethod print-object ((obj envelope) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ":POINTS ~D :LOOP-NODE ~D :RELEASE-NODE ~D"
            (envelope-points obj) (envelope-loop-node obj)
            (envelope-release-node obj))))

(defmethod free-p ((obj envelope))
  (null-pointer-p (envelope-data obj)))

(defmethod free ((obj envelope))
  (unless (free-p obj)
    (funcall (envelope-foreign-free obj) (envelope-data obj))
    (incudine-cancel-finalization obj)
    (setf (envelope-data-ptr obj) (null-pointer)
          (envelope-duration obj) +sample-zero+
          (envelope-points obj) 0
          (envelope-data-size obj) 0
          (envelope-loop-node obj) -1
          (envelope-release-node obj) -1
          (envelope-max-points obj) 0)
    (if (envelope-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-envelope-pool*)
        (incudine.util::free-object obj *envelope-pool*))
    (nrt-msg debug "Free ~A" (type-of obj)))
  (values))

(declaim (inline envelope-restart-level))
(defun envelope-restart-level (instance)
  "Return the restart level of the envelope or NIL if it is the value
of the level before to restart. Setfable."
  (envelope-%restart-level instance))

(declaim (inline set-envelope-restart-level))
(defun set-envelope-restart-level (instance value)
  (setf (envelope-%restart-level instance)
        (if value (sample value))))

(defsetf envelope-restart-level set-envelope-restart-level)

(declaim (inline compute-envelope-data-size))
(defun compute-envelope-data-size (size)
  (declare (type non-negative-fixnum size))
  (- (* size 3) 2))

(declaim (inline compute-envelope-points))
(defun compute-envelope-points (levels times)
  (declare (type list levels times))
  (max (length levels) (1+ (length times))))

(defun copy-envelope (envelope)
  "Return a copy of ENVELOPE."
  (declare (type envelope envelope))
  (if (free-p envelope)
      (incudine-error "The envelope is unusable.")
      (let* ((%points (envelope-points envelope))
             (%data-size (compute-envelope-data-size %points))
             (%max-points (max %points *envelope-default-max-points*))
             (max-data-size (compute-envelope-data-size %max-points))
             (rt-p (allow-rt-memory-p)))
        (declare (type non-negative-fixnum %points %data-size %max-points
                       max-data-size)
                 (type boolean rt-p))
        (multiple-value-bind (%data new free-fn pool)
            (reduce-warnings
              (if rt-p
                  (values (foreign-rt-alloc 'sample :count max-data-size)
                          (incudine.util::alloc-rt-object *rt-envelope-pool*)
                          #'safe-foreign-rt-free
                          *rt-envelope-pool*)
                  (values (foreign-alloc-sample max-data-size)
                          (incudine.util::alloc-object *envelope-pool*)
                          #'foreign-free
                          *envelope-pool*)))
          (declare (type foreign-pointer %data) (type envelope new)
                   (type incudine-object-pool pool))
          (incudine.util::with-struct-slots
              ((data-ptr data-size duration points max-points loop-node
                release-node %restart-level real-time-p foreign-free)
               new envelope "INCUDINE")
            (setf data-ptr %data
                  data-size %data-size
                  duration (envelope-duration envelope)
                  points %points
                  max-points %max-points
                  loop-node (envelope-loop-node envelope)
                  release-node (envelope-release-node envelope)
                  %restart-level (envelope-%restart-level envelope)
                  real-time-p rt-p
                  foreign-free free-fn)
            (foreign-copy-samples %data (envelope-data envelope) %data-size)
            (incudine-finalize new
              (lambda ()
                (funcall free-fn %data)
                (incudine-object-pool-expand pool 1)))
            new)))))

(defun check-envelope-points (env levels times)
  (declare (type envelope env) (type list levels times))
  (let ((size (compute-envelope-points levels times)))
    (when (/= size (envelope-points env))
      (setf (envelope-points env) size
            (envelope-data-size env) (compute-envelope-data-size size)))
    (when (> size (envelope-max-points env))
      (let ((real-time-p (envelope-real-time-p env))
            (free-function (envelope-foreign-free env)))
        (reduce-warnings
          (setf (envelope-data-ptr env)
                (if real-time-p
                    (rt-eval (:return-value-p t)
                      (foreign-rt-realloc (envelope-data env) 'sample
                        :count (compute-envelope-data-size size)))
                    (foreign-realloc (envelope-data-ptr env) 'sample
                      :count (compute-envelope-data-size size)))))
        (let ((data (envelope-data env))
              (pool (if real-time-p *rt-envelope-pool* *envelope-pool*)))
          (incudine-cancel-finalization env)
          (incudine-finalize env
            (lambda ()
              (funcall free-function data)
              (incudine-object-pool-expand pool 1)))
          (setf (envelope-max-points env) size))))
    env))

(declaim (inline exponential-curve-p))
(defun exponential-curve-p (curve)
  (member curve '(:exp :exponential)))

(declaim (inline exponential-curve-id-p))
(defun exponential-curve-id-p (index)
  (= index +seg-exp-func+))

;;; Default zero level in an exponential curve
(define-constant +exp-sample-zero+ (sample 1d-5))

(declaim (inline envelope-fix-zero))
(defun envelope-fix-zero (level curve)
  (if (and (exponential-curve-p curve) (zerop level))
      +exp-sample-zero+
      (sample level)))

(declaim (inline seg-function-spec->sample))
(defun seg-function-spec->sample (x)
  (typecase x
    (number (sample x))
    (keyword (case x
               (:step +seg-step-func+)
               ((:lin :linear) +seg-lin-func+)
               ((:exp :exponential) +seg-exp-func+)
               ((:sin :sine) +seg-sine-func+)
               ((:wel :welch) +seg-welch-func+)
               ((:sqr :square) +seg-square-func+)
               ((:cub :cubic) +seg-cubic-func+)
               (otherwise +seg-lin-func+)))
    (otherwise +seg-lin-func+)))

(declaim (inline sample->seg-function-spec))
(defun sample->seg-function-spec (x)
  (declare (type sample x)
           #+(or cmu sbcl) (values (or symbol sample)))
  (cond ((= x +seg-step-func+) :step)
        ((= x +seg-lin-func+) :linear)
        ((= x +seg-exp-func+) :exponential)
        ((= x +seg-sine-func+) :sine)
        ((= x +seg-welch-func+) :welch)
        ((= x +seg-square-func+) :square)
        ((= x +seg-cubic-func+) :cubic)
        (t x)))

(declaim (inline envelope-base->curve))
(defun envelope-base->curve (base lev0 lev1 min max)
  (let ((b1 (1- base))
        (b2 (- max (* base min))))
    (/ (+ (* lev1 b1) b2)
       (+ (* lev0 b1) b2))))

(defun envelope-base->curves (base levels)
  "Return the list of curvature values for the CURVE keyword in
MAKE-ENVELOPE related to an envelope's base in the style of CLM."
  (cond ((< base 0)
         (incudine-error "Envelope's base ~A less than zero." base))
        ((= base 0) (list :step))
        ((= base 1) (list :linear))
        (t
         (loop for y0 = (car levels) then y1
               for y1 in (cdr levels)
               with min = (reduce #'min levels)
                and max = (reduce #'max levels)
               collect (log (envelope-base->curve base y0 y1 min max))))))

(defun %edit-envelope (env levels times
                       &optional curve base loop-node release-node)
  (declare (type envelope env) (type list levels times)
           (type (or fixnum null) loop-node release-node))
  (let ((data (envelope-data
                (check-envelope-points env levels times)))
        (curve (cond (base (envelope-base->curves base levels))
                     ((null curve) (list :lin))
                     ((atom curve) (list curve))
                     (t curve)))
        (first-level (sample (car levels))))
    (setf (smp-ref data 0)
          (envelope-fix-zero first-level (car curve)))
    (setf (envelope-duration env) +sample-zero+)
    (when loop-node (setf (envelope-loop-node env) loop-node))
    (when release-node (setf (envelope-release-node env) release-node))
    (do ((i 1 (1+ i))
         (lev (cdr levels) (or (cdr lev) (cdr levels)))
         (tim times (or (cdr tim) times))
         (cur curve (or (cdr cur) curve)))
        ((= i (envelope-data-size env)))
      (declare (type positive-fixnum i) (type list lev tim cur))
      (incf (envelope-duration env)
            (setf (smp-ref data i) (sample (car tim))))
      (setf (smp-ref data (incf i))
            (envelope-fix-zero (car lev) (car cur)))
      (setf (smp-ref data (incf i))
            (seg-function-spec->sample (car cur))))
    env))

(defun* edit-envelope (env levels times linen perc cutoff asr adsr dadsr
                       (peak-level 1) curve base loop-node release-node
                       (restart-level nil restart-level-p))
  "Edit an ENVELOPE structure.

If LEVELS is non-NIL, change the list of LEVELS and the list of TIMES.

CURVE sets the shape of the segments; the possible values are :STEP,
:LIN or :LINEAR, :EXP or :EXPONENTIAL, :SIN or :SINE, :WEL or :WELCH,
:SQR or :SQUARE, :CUB or :CUBIC, a number that represents the curvature
value between two levels for all the segments or a list of the prior
values to specify the curvature values for each segment. CURVE is
ignored if BASE is non-NIL.

If the envelope is sustained, RELEASE-NODE specifies the point of the
release (starting from 0). -1 means 'envelope without sustain'.

If LOOP-NODE is a non-negative value, it is the starting point of the
loop of the segments during the sustain phase of the envelope. The
ending point is the point that precedes the release point RELEASE-NODE.

LINEN is NIL or a list (attack-time sustain-time release-time). CURVE
defaults to :LINEAR.

PERC is NIL or a list (attack-time release-time). CURVE defaults to -4.

CUTOFF is NIL or the release time. CURVE defaults to :EXPONENTIAL and
RELEASE-NODE is 0.

ASR is NIL or the list (attack-time sustain-level release-time).
CURVE and RELEASE-NODE default to -4 and 1 respectively.

ADSR is NIL or the list (attack-time decay-time sustain-level release-time).
CURVE and RELEASE-NODE default to -4 and 2 respectively.
SUSTAIN-LEVEL is a ratio of PEAK-LEVEL.

DADSR is NIL or the list (delay-time attack-time decay-time sustain-level release-time).
CURVE and RELEASE-NODE default to -4 and 3 respectively.
SUSTAIN-LEVEL is a ratio of PEAK-LEVEL.

PEAK-LEVEL is the peak level (1 by default) when LEVELS and ASR are NIL.

If RESTART-LEVEL is NIL, the envelope restarts from the current level
otherwise it restarts from the value RESTART-LEVEL."
  (declare (type envelope env)
           (type list levels times linen perc asr adsr dadsr)
           (type (or real null) cutoff base)
           (type real peak-level)
           (type (or fixnum null) loop-node release-node)
           (type (or real null) restart-level))
  (when restart-level-p
    (setf (envelope-restart-level env) restart-level))
  (cond (levels
         (%edit-envelope env levels times curve base loop-node release-node))
        (linen
         (%edit-envelope env (list 0 peak-level peak-level 0) linen
                         (or curve :lin) base))
        (perc
         (%edit-envelope env (list 0 peak-level 0) perc (or curve -4) base))
        (cutoff
         (%edit-envelope env (list peak-level 0) (list cutoff)
                         (or curve :exp) base -1 0))
        (asr
         (destructuring-bind (a s r) asr
           (%edit-envelope env (list 0 s 0) (list a r) (or curve -4) base -1 1)))
        (adsr
         (destructuring-bind (a d s r) adsr
           (%edit-envelope env (list 0 peak-level (* peak-level s) 0)
                           (list a d r) (or curve -4) base -1 2)))
        (dadsr
         (destructuring-bind (dt a d s r) dadsr
           (%edit-envelope env (list 0 0 peak-level (* peak-level s) 0)
                           (list dt a d r) (or curve -4) base -1 3)))
        (t env)))

(defmacro curve-case (keyform &body cases)
  (with-gensyms (curve)
    `(let ((,curve ,keyform))
       (declare (ignorable ,curve))
       (cond ,@(mapcar (lambda (x)
                         `(,(if (eq (car x) 'otherwise)
                                t
                                `(= ,curve ,(car x)))
                           ,@(cdr x)))
                       cases)))))

(defmacro %%segment-init (beg end dur curve grow a2 b1 y1 y2)
  (with-gensyms (w diff)
    `(curve-case ,curve
       (+seg-step-func+ (values))
       (+seg-lin-func+ (setf ,grow (/ (- ,end ,beg) ,dur)))
       (+seg-exp-func+
        (setf ,grow (expt (the non-negative-sample (/ ,end ,beg))
                          (/ 1.0 ,dur))))
       (+seg-sine-func+
        (let ((,w (/ pi ,dur)))
          (setf ,a2 (* (+ ,end ,beg) 0.5)
                ,b1 (* 2.0 (cos (the maybe-limited-sample ,w)))
                ,y1 (* (- ,end ,beg) 0.5)
                ,y2 (* ,y1 (sin (the maybe-limited-sample
                                  (- +half-pi+ ,w)))))))
       (+seg-welch-func+
        (let ((,w (/ +half-pi+ ,dur))
              (,diff (- ,beg ,end)))
          (setf ,b1 (* 2.0 (cos (the maybe-limited-sample ,w))))
          (if (>= ,end ,beg)
              (setf ,a2 ,beg
                    ,y1 +sample-zero+
                    ,y2 (* ,diff (sin (the maybe-limited-sample ,w))))
              (setf ,a2 ,end
                    ,y1 ,diff
                    ,y2 (* ,diff (cos (the maybe-limited-sample ,w)))))))
       (+seg-square-func+
        (setf ,y1 (the non-negative-sample (sqrt ,beg))
              ,y2 (the non-negative-sample (sqrt ,end))
              ,grow (/ (- ,y2 ,y1) ,dur)))
       (+seg-cubic-func+
        (setf ,y1 (expt (the non-negative-sample ,beg) ,(sample 1/3))
              ,y2 (expt (the non-negative-sample ,end) ,(sample 1/3))
              ,grow (/ (- ,y2 ,y1) ,dur)))
       ;; custom curve
       (otherwise (if (< (abs ,curve) 0.001)
                      (setf ,grow (/ (- ,end ,beg) ,dur)
                            ,curve +seg-lin-func+)
                      (setf ,b1 (/ (- ,end ,beg) (- 1.0 (exp ,curve)))
                            ,a2 (+ ,beg ,b1)
                            ,grow (exp (/ ,curve ,dur))))))))

(defmacro %segment-init (beg end dur curve grow a2 b1 y1 y2)
  (apply-sample-coerce
    (macroexpand-1
      `(%%segment-init ,beg ,end ,dur ,curve ,grow ,a2 ,b1 ,y1 ,y2))))

(defmacro with-segment-stack (vars consointer &body body)
  (with-gensyms (stack)
    `(let ((,stack (car ,consointer)))
       (declare (type foreign-pointer ,stack))
       (symbol-macrolet ,(loop for v in vars
                               for i from 0
                               collect `(,v (smp-ref ,stack ,i)))
         ,@body))))

(defun segment-stack-init (consointer dur)
  (declare (type cons consointer)
           (type non-negative-fixnum dur)
           #.*standard-optimize-settings*)
  (with-segment-stack (beg end curve grow a2 b1 y1 y2) consointer
    (%segment-init beg end dur curve grow a2 b1 y1 y2)
    (values)))

(defun %segment-stack-reposition (consointer dur delta)
  (declare (type cons consointer)
           (type non-negative-fixnum dur)
           (type fixnum delta)
           #.*standard-optimize-settings*)
  (with-segment-stack (beg end curve grow a2 b1 y1 y2) consointer
    (curve-case curve
       (+seg-step-func+ beg)
       (+seg-lin-func+
        (setf grow (/ (- end beg) dur))
        (incf beg (* grow delta)))
       (+seg-exp-func+
        (let ((x (the non-negative-sample (/ end beg)))
              (y (/ (sample 1) dur)))
          (setf grow (expt x y))
          (setf beg (* beg (expt x (* y delta))))))
       (+seg-sine-func+
        (let ((w (/ pi dur))
              (a3 (* (- end beg) 0.5)))
          (setf a2 (* (+ end beg) 0.5))
          (setf b1 (* 2.0 (cos (the maybe-limited-sample w))))
          (setf y2 (- a2 (+ beg (* a3 (- 1 (cos (the maybe-limited-sample
                                                  (* w (the fixnum
                                                         (1- delta))))))))))
          (incf beg (* a3 (- 1 (cos (the maybe-limited-sample (* w delta))))))
          (setf y1 (- a2 beg))))
       (+seg-welch-func+
        (let ((w (/ +half-pi+ dur))
              (a3 (- end beg)))
          (setf a2 beg)
          (setf b1 (* 2.0 (cos (the maybe-limited-sample w))))
          (setf y2 (* a3 (sin (the maybe-limited-sample
                                (* w (the fixnum (1- delta)))))))
          (incf beg (* a3 (sin (the maybe-limited-sample (* w delta)))))
          (setf y1 (- beg a2))))
       (+seg-square-func+
        (setf y1 (the non-negative-sample (sqrt beg))
              y2 (the non-negative-sample (sqrt end))
              grow (/ (- y2 y1) dur)
              y1 (+ y1 (* grow delta))
              beg (* y1 y1)))
       (+seg-cubic-func+
        (setf y1 (expt (the non-negative-sample beg) (sample 1/3))
              y2 (expt (the non-negative-sample end) (sample 1/3))
              grow (/ (- y2 y1) dur)
              y1 (+ y1 (* grow delta))
              beg (* y1 y1 y1)))
       ;; custom curve
       (otherwise
        (if (< (abs curve) 0.001)
            (setf grow (/ (- end beg) dur)
                  curve +seg-lin-func+
                  beg (+ beg (* grow delta)))
            (setf b1 (/ (- end beg) (- 1.0 (exp curve)))
                  a2 (+ beg b1)
                  grow (exp (/ curve dur))
                  b1 (* b1 (expt (the non-negative-sample grow) (sample delta)))
                  beg (- a2 b1)))))
    (values)))

(declaim (inline segment-stack-reposition))
(defun segment-stack-reposition (consointer dur delta)
  (declare (type cons consointer)
           (type non-negative-fixnum dur)
           (type fixnum delta)
           #.*standard-optimize-settings*)
  (if (= delta 0)
      (segment-stack-init consointer dur)
      (%segment-stack-reposition consointer dur delta)))

(defmacro %segment-update-level (level curve grow a2 b1 y1 y2)
  (with-gensyms (y0)
    `(curve-case ,curve
       (+seg-step-func+ ,level)
       (+seg-lin-func+ (setf ,level (+ ,level ,grow)))
       (+seg-exp-func+ (setf ,level (* ,level ,grow)))
       (+seg-sine-func+
        (let ((,y0 (- (* ,b1 ,y1) ,y2)))
          (setf ,level (- ,a2 ,y0) ,y2 ,y1 ,y1 ,y0)))
       (+seg-welch-func+
        (let ((,y0 (- (* ,b1 ,y1) ,y2)))
          (setf ,level (+ ,a2 ,y0) ,y2 ,y1 ,y1 ,y0)))
       (+seg-square-func+
        (setf ,y1 (+ ,y1 ,grow)
              ,level (* ,y1 ,y1)))
       (+seg-cubic-func+
        (setf ,y1 (+ ,y1 ,grow)
              ,level (* ,y1 ,y1 ,y1)))
       ;; custom curve
       (otherwise (setf ,b1 (* ,b1 ,grow)
                        ,level (- ,a2 ,b1))))))

(defun make-envelope (levels times &key curve base (loop-node -1)
                      (release-node -1) restart-level
                      (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure from a list of LEVELS and
a list of TIMES in seconds.

If BASE is a number, it is the envelope's base in the style of CLM
(Common Lisp Music), where BASE is e^k and the curvature depends on
the highest and lowest levels.

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
  (declare (type list levels times)
           (type fixnum loop-node release-node)
           (type boolean real-time-p))
  (let* ((size (max (length levels) (1+ (length times))))
         (%max-points (max size *envelope-default-max-points*))
         (max-data-size (compute-envelope-data-size %max-points))
         (rt-p (and real-time-p *allow-rt-memory-pool-p*)))
    (declare (type non-negative-fixnum size %max-points max-data-size)
             #.*reduce-warnings*)
    (multiple-value-bind (%data env free-fn pool)
        (if rt-p
            (values (foreign-rt-alloc 'sample :count max-data-size)
                    (incudine.util::alloc-rt-object *rt-envelope-pool*)
                    #'safe-foreign-rt-free
                    *rt-envelope-pool*)
            (values (foreign-alloc-sample max-data-size)
                    (incudine.util::alloc-object *envelope-pool*)
                    #'foreign-free
                    *envelope-pool*))
      (declare (type foreign-pointer %data) (type envelope env)
               (type incudine-object-pool pool))
      (handler-case
          (incudine.util::with-struct-slots
              ((data-ptr data-size points max-points %restart-level real-time-p
                foreign-free) env envelope "INCUDINE")
            (setf data-ptr %data
                  data-size (compute-envelope-data-size size)
                  points size
                  max-points %max-points
                  %restart-level (and restart-level (sample restart-level))
                  real-time-p rt-p
                  foreign-free free-fn)
            (edit-envelope env levels times :curve curve :base base
                           :loop-node loop-node :release-node release-node))
        (condition (c)
          (funcall free-fn %data)
          (incudine-object-pool-expand pool 1)
          (error c)))
      (incudine-finalize env
        (lambda ()
          (funcall free-fn %data)
          (incudine-object-pool-expand pool 1))))))

(declaim (inline check-envelope-node))
(defun check-envelope-node (env number)
  (declare (type non-negative-fixnum number))
  (and (>= number 0) (< number (envelope-points env))))

(declaim (inline envelope-level))
(defun envelope-level (env node-number)
  "Return the level of the envelope ENV at NODE-NUMBER. Setfable."
  (declare (type envelope env) (type non-negative-fixnum node-number))
  (smp-ref (envelope-data env)
           (reduce-warnings
             (if (plusp node-number)
                 (the non-negative-fixnum
                   (1- (* (min node-number
                               (1- (the positive-fixnum (envelope-points env))))
                          3)))
                 0))))

(defun envelope-fix-zero-p (env node-number curve-index)
  (declare (type envelope env)
           (type non-negative-fixnum node-number curve-index))
  (or (and (> node-number 0)
           (exponential-curve-id-p (smp-ref (envelope-data env) curve-index)))
      (and (/= node-number (1- (envelope-points env)))
           (exponential-curve-id-p
             (smp-ref (envelope-data env) (+ curve-index 3))))))

(declaim (inline set-envelope-level))
(defun set-envelope-level (env node-number level)
  (declare (type envelope env) (type non-negative-fixnum node-number)
           (type number level))
  (if (check-envelope-node env node-number)
      (let ((curve-index (* node-number 3)))
        (declare (type non-negative-fixnum curve-index))
        (setf (smp-ref (envelope-data env)
                       (if (> node-number 0) (1- curve-index) 0))
              (if (and (zerop level)
                       (envelope-fix-zero-p env node-number curve-index))
                  +exp-sample-zero+
                  (sample level))))
      +sample-zero+))

(defsetf envelope-level set-envelope-level)

(declaim (inline envelope-time))
(defun envelope-time (env node-number)
  "Return the time of the envelope ENV at NODE-NUMBER. Setfable."
  (declare (type envelope env) (type non-negative-fixnum node-number))
  (if (< 0 node-number (envelope-points env))
      (smp-ref (envelope-data env)
               (the non-negative-fixnum (- (* node-number 3) 2)))
      +sample-zero+))

(declaim (inline set-envelope-time))
(defun set-envelope-time (env node-number time)
  (declare (type envelope env) (type (unsigned-byte 16) node-number)
           (type number time))
  (if (< 0 node-number (envelope-points env))
      (setf (smp-ref (envelope-data env)
                     (the positive-fixnum (- (* node-number 3) 2)))
            (sample time))
      +sample-zero+))

(defsetf envelope-time set-envelope-time)

(declaim (inline envelope-curve))
(defun envelope-curve (env node-number)
  "Return the curvature of the envelope ENV at NODE-NUMBER. Setfable."
  (declare (type envelope env) (type non-negative-fixnum node-number)
           #+(or cmu sbcl) (values (or symbol sample)))
  (incudine-optimize
    (declare #.*reduce-warnings*)
    (when (< 0 node-number (envelope-points env))
      (sample->seg-function-spec
        (smp-ref (envelope-data env)
                 (the non-negative-fixnum (* 3 node-number)))))))

(defun set-envelope-curve (env node-number curve)
  (declare (type envelope env) (type non-negative-fixnum node-number)
           (type (or symbol real) curve)
           #+(or cmu sbcl) (values (or symbol real)))
  (incudine-optimize
    (macrolet ((segment-fix-zero (ptr new-zero old-zero)
                 `(when (= (smp-ref ,ptr 0) ,old-zero)
                    (setf (smp-ref ,ptr 0) ,new-zero)))
               (segment-fix-zeros (beg-ptr end-ptr new-zero old-zero)
                 `(progn
                    (segment-fix-zero ,beg-ptr ,new-zero ,old-zero)
                    (segment-fix-zero ,end-ptr ,new-zero ,old-zero))))
      (when (< 0 node-number (the non-negative-fixnum (envelope-points env)))
        (let* ((curve-ptr (inc-pointer (envelope-data env)
                                       (the non-negative-fixnum
                                         (* node-number 3
                                            +foreign-sample-size+))))
               (end-ptr (inc-pointer curve-ptr (- +foreign-sample-size+)))
               (beg-ptr (if (= node-number 1)
                            (envelope-data env)
                            (inc-pointer end-ptr
                                         (* -3 +foreign-sample-size+)))))
          (if (reduce-warnings (exponential-curve-p curve))
              (segment-fix-zeros beg-ptr end-ptr +exp-sample-zero+ 0)
              (segment-fix-zeros beg-ptr end-ptr +sample-zero+
                                 +exp-sample-zero+))
          (setf (smp-ref curve-ptr 0)
                (reduce-warnings (seg-function-spec->sample curve)))
          curve)))))

(defsetf envelope-curve set-envelope-curve)

(defun envelope-levels (env)
  (loop for i below (envelope-points env) collect (envelope-level env i)))

(defun set-envelope-base (env base)
  "Set the curvature of the envelope in the style of CLM, where BASE
is e^k and the curvature depends on the highest and lowest levels."
  (declare (type envelope env) (type real base))
  (let ((curves (envelope-base->curves base (envelope-levels env))))
    (declare #.*standard-optimize-settings*)
    (loop for c in curves
          for i of-type non-negative-fixnum from 1
          do (setf (envelope-curve env i) c))
    base))

(defmacro %envelope-at (beg end pos curve tmp0 tmp1 tmp2)
  (with-gensyms (sqrt-s expt-s x power)
    (apply-sample-coerce
     `(flet ((,sqrt-s (,x) (sqrt (the non-negative-sample ,x)))
             (,expt-s (,x ,power) (expt (the non-negative-sample ,x) ,power)))
        (curve-case ,curve
          (+seg-step-func+ ,beg)
          (+seg-lin-func+ (+ (* ,pos (- ,end ,beg)) ,beg))
          (+seg-exp-func+ (* ,beg (,expt-s (/ ,end ,beg) ,pos)))
          (+seg-sine-func+
           (+ ,beg (* (- ,end ,beg)
                      (+ (* (- (cos (* pi ,pos))) 0.5) 0.5))))
          (+seg-welch-func+
           (if (< ,beg ,end)
               (+ ,beg (* (- ,end ,beg) (sin (* +half-pi+ ,pos))))
               (- ,end (* (- ,end ,beg) (sin (- +half-pi+
                                                (* +half-pi+ ,pos)))))))
          (+seg-square-func+ (setf ,tmp0 (,sqrt-s ,beg)
                                   ,tmp1 (,sqrt-s ,end)
                                   ,tmp2 (+ (* pos (- ,tmp1 ,tmp0)) ,tmp0))
                             (* ,tmp2 ,tmp2))
          (+seg-cubic-func+ (setf ,tmp0 (,expt-s ,beg ,(sample 1/3))
                                  ,tmp1 (,expt-s ,end ,(sample 1/3))
                                  ,tmp2 (+ (* ,pos (- ,tmp1 ,tmp0)) ,tmp0))
                            (* ,tmp2 ,tmp2 ,tmp2))
          (otherwise (if (< (abs ,curve) 0.001)
                         ;; Linear segment
                         (+ (* ,pos (- ,end ,beg)) ,beg)
                         (+ ,beg (* (- ,end ,beg)
                                    (/ (- 1.0 (exp (* ,pos ,curve)))
                                       (- 1.0 (exp ,curve))))))))))))

(defun envelope-at (env time)
  "Return the level of the envelope ENV at time TIME."
  (declare (type envelope env) (type (real 0) time))
  (if (zerop time)
      (envelope-level env 0)
      ;; Temporary C variables to avoid consing with DOUBLE-FLOAT
      (with-samples ((pos-time time)
                     (delta-time 0.0)
                     (t0 0.0)
                     (t1 0.0)
                     (tmp0 0.0)
                     (tmp1 0.0)
                     (tmp2 0.0))
        (let ((last-point-index (1- (envelope-points env)))
              (data (envelope-data env)))
          (declare #.*standard-optimize-settings* #.*reduce-warnings*)
          (labels ((look (p index)
                     (declare (type non-negative-fixnum p index))
                     (setf delta-time (smp-ref data (- index 2)))
                     (setf t0 t1)
                     (incf t1 delta-time)
                     (if (< pos-time t1)
                         (env-at (smp-ref data (if (= p 1) 0 (- index 4)))
                                 (smp-ref data (- index 1))
                                 (/ (- pos-time t0) delta-time)
                                 (smp-ref data index))
                         (if (< p last-point-index)
                             (look (1+ p) (+ index 3))
                             ;; Return the last level
                             (smp-ref data (- index 1)))))
                   (env-at (beg end pos curve)
                     (%envelope-at beg end pos curve tmp0 tmp1 tmp2)))
            (look 1 3))))))

(defun scale-envelope (env mult)
  "Multiply the levels of the envelope by MULT."
  (declare (type envelope env) (type real mult))
  (let ((points (envelope-points env))
        (data (envelope-data env)))
    (setf (envelope-level env 0) (* (smp-ref data 0) mult))
    (loop for i from 1 below points do
         (setf (envelope-level env i)
               (* (smp-ref data (1- (* i 3))) mult)))
    env))

(defun normalize-envelope (env value)
  "Scale the levels of the envelope to be between -VALUE and VALUE."
  (declare (type envelope env) (type real value))
  (let ((points (envelope-points env)))
    (declare (type positive-fixnum points))
    (labels ((norm (index maxval)
               (declare (type non-negative-fixnum index)
                        (type sample maxval))
               (if (= index points)
                   maxval
                   (norm (1+ index)
                         (max (abs (envelope-level env index)) maxval)))))
      (scale-envelope env (/ value (norm 1 (abs (envelope-level env 0))))))))

(defun rescale-envelope (env min max)
  "Rescale the levels of the envelope to be between MIN and MAX."
  (declare (type envelope env) (type real min max))
  (let ((points (envelope-points env)))
    (declare (type non-negative-fixnum points))
    (labels ((resc (index old-min old-max)
               (declare (type non-negative-fixnum index)
                        (type sample old-min old-max))
               (if (>= index points)
                   (values (/ (sample 1) (- old-max old-min)) old-min)
                   (let ((value (smp-ref (envelope-data env) index)))
                     (resc (+ index 3)
                           (min value old-min)
                           (max value old-max))))))
      (multiple-value-bind (old-delta old-min)
          (let ((init (envelope-level env 0)))
            (resc 2 init init))
        (let ((new-delta (- max min)))
          (flet ((set-level (index offset)
                   (setf (envelope-level env index)
                         (+ min (* new-delta old-delta
                                   (- (smp-ref (envelope-data env)
                                               offset)
                                      old-min))))))
            (set-level 0 0)
            (labels ((set-levels (index offset)
                       (declare (type non-negative-fixnum index offset))
                       (cond ((>= index points) env)
                             (t (set-level index offset)
                                (set-levels (1+ index) (+ offset 3))))))
              (set-levels 1 2))))))))

(defun breakpoint-sequence-p (seq)
  (if (or (arrayp seq) (consp seq))
      (let ((len (length seq)))
        (values (evenp len) len))
      (values nil 0)))

(defun expand-curve-list (curve points)
  (labels ((complete-curve-list (lst curr result size)
             (if (zerop size)
                 (nreverse result)
                 (complete-curve-list lst (or (cdr curr) lst)
                                      (cons (car curr) result)
                                      (1- size)))))
    (if (and (consp curve) (/= (length curve) (1- points)))
        (complete-curve-list curve curve nil (1- points))
        curve)))

(declaim (inline expanded-atomic-curve-plus-linear))
(defun expanded-atomic-curve-plus-linear (curve points)
  (do ((i 0 (1+ i))
       (acc nil)
       (end (- points 2)))
      ((>= i end) (nreverse (cons :lin acc)))
    (push curve acc)))

(defun normalize-env-times (lst dur)
  (if dur
      (let ((scl (/ (car (last lst 2)) dur)))
        (if (or (= scl 0) (< (car lst) 0))
            (incudine-error "Malformed breakpoint list pairs.")
            (loop for i on lst by #'cddr
                  collect (/ (car i) scl)
                  collect (cadr i))))
      lst))

(defun adjust-breakpoint-list (lst scaler offset)
  (loop for (x y0) on lst by #'cddr
        for y = (let ((v (if (= scaler 1) y0 (* y0 scaler))))
                  (if (= offset 0) v (+ v offset)))
        collect x collect y))

(defun breakpoint-levels-and-times (lst)
  (let ((levels) (times))
    (loop for t0 = 0 then t1
          for (t1 y) on lst by #'cddr do
            (push (- t1 t0) times)
            (push y levels)
          finally (return (values (nreverse levels) (cdr (nreverse times)))))))

(defun breakpoints->env-arguments (bp-seq curve base scaler offset duration)
  (declare (type (or list array) bp-seq))
  (multiple-value-bind (bp-seq-p size)
      (breakpoint-sequence-p bp-seq)
    (declare (type non-negative-fixnum size))
    (if bp-seq-p
        (let ((bplist (let ((lst (coerce bp-seq 'list)))
                        (normalize-env-times
                          (if (or scaler offset)
                              (adjust-breakpoint-list lst (or scaler 1)
                                                      (or offset 0))
                              lst)
                          duration))))
          (declare #.*standard-optimize-settings*)
          (unless (reduce-warnings (zerop (car bplist)))
            ;; Add the first pair.
            (setf bplist (cons 0 (cons (cadr bplist) bplist)))
            (when (and curve (null base))
              (let ((points (1- (the positive-fixnum (/ size 2)))))
                (declare (type positive-fixnum points))
                ;; Add the first curve.
                (setf curve (if (consp curve)
                                (cons :lin (expand-curve-list curve points))
                                (cons :lin (loop repeat points
                                                 collect curve)))))))
          (multiple-value-bind (levels times)
              (breakpoint-levels-and-times bplist)
            (values levels times (and (null base) curve))))
        (incudine-error "Wrong breakpoint sequence: ~A" bp-seq))))

(defun breakpoints->env (bp-seq &key curve base scaler offset duration
                         (loop-node -1) (release-node -1)
                         restart-level (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE from a sequence of break-point pairs.

If BASE is a number, it is e^k and the curvature depends on the highest
and lowest levels.

If SCALER is non-NIL, the levels of the envelope are scaled by that value.

If OFFSET is non-NIL, it is the value added to the levels of the envelope.

If DURATION is non-NIL, it is the duration of the envelope in seconds.

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
  (declare (type (or list array) bp-seq))
  (multiple-value-bind (levels times curve)
      (breakpoints->env-arguments bp-seq curve base scaler offset duration)
    (make-envelope levels times :curve curve :base base
                   :loop-node loop-node
                   :release-node release-node
                   :restart-level restart-level
                   :real-time-p real-time-p)))

(defun freq-breakpoints->env (bp-seq &key (freq-max (* *sample-rate* 0.5))
                              curve base (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE from a sequence of break-point
pairs interpreted as frequency response.

FREQ-MAX is the highest frequency and defaults to the half of *SAMPLE-RATE*.

If BASE is a number, it is e^k and the curvature depends on the
highest and lowest levels.

CURVE sets the shape of the segments; the possible values are :STEP,
:LIN or :LINEAR (default), :EXP or :EXPONENTIAL, :SIN or :SINE, :WEL
or :WELCH, :SQR or :SQUARE, :CUB or :CUBIC, a number that represents
the curvature value between two levels for all the segments or a list
of the prior values to specify the curvature values for each segment.
CURVE is ignored if BASE is non-NIL.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type (or list array) bp-seq))
  (multiple-value-bind (bp-seq-p size)
      (breakpoint-sequence-p bp-seq)
    (declare (type non-negative-fixnum size))
    (if bp-seq-p
        (let* ((points (/ size 2))
               (curve (and (null base) (expand-curve-list curve points)))
               (bplist
                 (do ((acc nil (list* (second l) (first l) acc))
                      (l (coerce bp-seq 'list) (cddr l)))
                     ((or (null l) (> (first l) freq-max))
                      (when l
                        ;; Discard useless points.
                        (let ((len (/ (length l) 2)))
                          (decf points len)
                          (when (and (consp curve) (> len 1))
                            (setf curve (butlast curve (1- len))))))
                      (unless (and (rest acc) (= (second acc) freq-max))
                        ;; Add the last pair.
                        (setf acc (list* (or (first acc) 0) freq-max acc))
                        (incf points)
                        (when (and curve (null l))
                          (setf curve
                                (if (consp curve)
                                    (nconc curve (list :lin))
                                    (expanded-atomic-curve-plus-linear
                                      curve points)))))
                      (nreverse acc)))))
          (breakpoints->env bplist :curve curve :base base
                            :real-time-p real-time-p))
        (incudine-error "Wrong breakpoint sequence: ~A" bp-seq))))

;;; Frequently used envelope shapes

(declaim (inline make-linen))
(defun make-linen (attack-time sustain-time release-time &key (level 1)
                   restart-level (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with peak LEVEL and a
straight line rise and decay pattern by default."
  (make-envelope (list 0 level level 0)
                 (list attack-time sustain-time release-time)
                 :curve :lin :restart-level restart-level
                 :real-time-p real-time-p))

(declaim (inline make-perc))
(defun make-perc (attack-time release-time
                  &key (level 1) (curve -4) base restart-level
                  (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with peak LEVEL,
ATTACK-TIME and RELEASE-TIME.

The curvature CURVE defaults to -4."
  (make-envelope (list 0 level 0) (list attack-time release-time)
                 :curve curve :base base :restart-level restart-level
                 :real-time-p real-time-p))

(declaim (inline make-cutoff))
(defun make-cutoff (release-time &key (level 1) (curve :exp) base restart-level
                    (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with peak LEVEL and RELEASE-TIME.

The curvature CURVE defaults to :EXPONENTIAL."
  (make-envelope (list level 0) (list release-time)
                 :curve curve :base base :release-node 0
                 :restart-level restart-level :real-time-p real-time-p))

(declaim (inline make-asr))
(defun make-asr (attack-time sustain-level release-time
                 &key (curve -4) base restart-level
                 (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with ATTACK-TIME, SUSTAIN-LEVEL
and RELEASE-TIME.

The curvature CURVE defaults to -4."
  (make-envelope (list 0 sustain-level 0) (list attack-time release-time)
                 :curve curve :base base :release-node 1
                 :restart-level restart-level :real-time-p real-time-p))

(declaim (inline make-adsr))
(defun make-adsr (attack-time decay-time sustain-level release-time
                  &key peak-level (curve -4) base restart-level
                  (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with ATTACK-TIME,
PEAK-LEVEL, DECAY-TIME, SUSTAIN-LEVEL and RELEASE-TIME.

PEAK-LEVELS defaults to 1.

SUSTAIN-LEVEL is a ratio of PEAK-LEVEL for compatibility with
Env.adsr in SuperCollider.

The curvature CURVE defaults to -4.

Other available keyword arguments are BASE, RESTART-LEVEL and
REAL-TIME-P. See MAKE-ENVELOPE for details."
  (multiple-value-bind (peak-level sustain-level)
      (if peak-level
          (values peak-level (* peak-level sustain-level))
          (values 1 sustain-level))
    (make-envelope (list 0 peak-level sustain-level 0)
                   (list attack-time decay-time release-time)
                   :curve curve :base base :release-node 2
                   :restart-level restart-level :real-time-p real-time-p)))

(declaim (inline make-dadsr))
(defun make-dadsr (delay-time attack-time decay-time sustain-level
                   release-time &key peak-level (curve -4) base
                   restart-level (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with DELAY-TIME,
ATTACK-TIME, PEAK-LEVEL, DECAY-TIME, SUSTAIN-LEVEL and RELEASE-TIME.

PEAK-LEVELS defaults to 1.

SUSTAIN-LEVEL is a ratio of PEAK-LEVEL for compatibility with
Env.adsr in SuperCollider.

The curvature CURVE defaults to -4.

Other available keyword arguments are BASE, RESTART-LEVEL and
REAL-TIME-P. See MAKE-ENVELOPE for details."
  (multiple-value-bind (peak-level sustain-level)
      (if peak-level
          (values peak-level (* peak-level sustain-level))
          (values 1 sustain-level))
    (make-envelope (list 0 0 peak-level sustain-level 0)
                   (list delay-time attack-time decay-time release-time)
                   :curve curve :base base :release-node 3
                   :restart-level restart-level :real-time-p real-time-p)))
