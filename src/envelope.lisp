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

(in-package :incudine)

;;; ENVELOPE is inspired by the Env object in SuperCollider

(defvar *envelope-default-max-points* 8)
(declaim (type non-negative-fixnum *envelope-default-max-points*))

;;; Values of the constants used in the past to get an index from 0 to 6
(define-constant +seg-step-func+   (sample  5e15))
(define-constant +seg-lin-func+    (sample  1e16))
(define-constant +seg-exp-func+    (sample  2e16))
(define-constant +seg-sine-func+   (sample  4e16))
(define-constant +seg-welch-func+  (sample  8e16))
(define-constant +seg-square-func+ (sample 16e16))
(define-constant +seg-cubic-func+  (sample 32e16))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (envelope (:constructor %make-envelope)
                       (:copier nil))
    (data (null-pointer) :type foreign-pointer)
    (duration +sample-zero+ :type sample)
    (points 0 :type non-negative-fixnum)
    (data-size 0 :type non-negative-fixnum)
    (loop-node -1 :type fixnum)
    (release-node -1 :type fixnum)
    (max-points *envelope-default-max-points* :type non-negative-fixnum)
    (real-time-p nil :type boolean)
    (foreign-free #'foreign-free :type function)))

(defmethod print-object ((obj envelope) stream)
  (format stream "#<~S :POINTS ~D :LOOP-NODE ~D :RELEASE-NODE ~D>"
          (type-of obj)
          (envelope-points obj) (envelope-loop-node obj)
          (envelope-release-node obj)))

(defmethod free ((obj envelope))
  (unless (null-pointer-p #1=(envelope-data obj))
    (funcall (envelope-foreign-free obj) #1#)
    (tg:cancel-finalization obj))
  (setf #1# (null-pointer)
        (envelope-duration obj) +sample-zero+
        (envelope-points obj) 0
        (envelope-data-size obj) 0
        (envelope-loop-node obj) -1
        (envelope-release-node obj) -1
        (envelope-max-points obj) 0)
  (values))

(declaim (inline calc-data-size))
(defun calc-data-size (size)
  (declare (type non-negative-fixnum size))
  (- (* size 3) 2))

(declaim (inline calc-envelope-points))
(defun calc-envelope-points (levels times)
  (declare (type list levels times))
  (max (length levels) (1+ (length times))))

(defun check-envelope-points (env levels times)
  (declare (type envelope env) (type list levels times))
  (let ((size (calc-envelope-points levels times)))
    (when (/= size (envelope-points env))
      (setf (envelope-points env) size
            (envelope-data-size env) (calc-data-size size)))
    (when (> size (envelope-max-points env))
      (tg:cancel-finalization env)
      (let ((real-time-p (envelope-real-time-p env))
            (free-function (envelope-foreign-free env)))
        (if real-time-p
            (foreign-rt-realloc (envelope-data env) 'sample
                                :count (calc-data-size size))
            (foreign-realloc-sample (envelope-data env)
                                    (calc-data-size size)))
        (setf (envelope-max-points env) size)
        (let ((data (envelope-data env)))
          (tg:finalize env (lambda ()
                             (rt-eval-if (real-time-p)
                               (funcall free-function data)))))))
    env))

(declaim (inline exponential-curve-p))
(defun exponential-curve-p (curve)
  (or (member curve '(:exp :exponential))
      (and (numberp curve) (/= curve 0))))

(declaim (inline exponential-curve-index-p))
(defun exponential-curve-index-p (index)
  (or (< index +seg-step-func+)
      (= index +seg-exp-func+)))

(declaim (inline envelope-fix-zero))
(defun envelope-fix-zero (level curve)
  (sample (if (and (exponential-curve-p curve)
                   (zerop level))
              1.0e-5
              level)))

(defun set-envelope (env levels times &key curve
                     (loop-node -1) (release-node -1))
  (declare (type envelope env) (type list levels times))
  (let ((data (envelope-data
               (check-envelope-points env levels times)))
        (curve (cond ((null curve) (list :lin))
                     ((atom curve) (list curve))
                     (t curve)))
        (first-level (sample (car levels))))
    (setf (smp-ref data 0)
          (envelope-fix-zero first-level (car curve)))
    (setf (envelope-duration env) +sample-zero+
          (envelope-loop-node env) loop-node
          (envelope-release-node env) release-node)
    (do ((i 1 (1+ i))
         (lev (cdr levels) (or (cdr lev) (cdr levels)))
         (tim times (or (cdr tim) times))
         (cur curve (or (cdr cur) curve)))
        ((= i (envelope-data-size env)))
      (declare (type positive-fixnum i)
               (type list lev tim cur))
      (incf (envelope-duration env)
            (setf (smp-ref data i) (sample (car tim))))
      (setf (smp-ref data (incf i))
            (envelope-fix-zero (car lev) (car cur)))
      (setf (smp-ref data (incf i))
            (seg-function-spec->sample (car cur))))
    env))

(declaim (inline seg-function-spec->sample))
(defun seg-function-spec->sample (x)
  (cond ((numberp x) (sample x))
        ((keywordp x)
         (case x
           (:step +seg-step-func+)
           ((:lin :linear) +seg-lin-func+)
           ((:exp :exponential) +seg-exp-func+)
           ((:sin :sine) +seg-sine-func+)
           ((:wel :welch) +seg-welch-func+)
           ((:sqr :square) +seg-square-func+)
           ((:cub :cubic) +seg-cubic-func+)
           (otherwise +seg-lin-func+)))
        (t +seg-lin-func+)))

(declaim (inline sample->seg-function-spec))
(defun sample->seg-function-spec (x)
  (declare (type sample x)
           #+(or cmu sbcl) (values (or symbol sample)))
  (cond ((= x +seg-step-func+)   :step)
        ((= x +seg-lin-func+)    :linear)
        ((= x +seg-exp-func+)    :exponential)
        ((= x +seg-sine-func+)   :sine)
        ((= x +seg-welch-func+)  :welch)
        ((= x +seg-square-func+) :square)
        ((= x +seg-cubic-func+)  :cubic)
        (t x)))

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
  (with-gensyms (w)
    `(curve-case ,curve
       (+seg-step-func+ (values))
       (+seg-lin-func+
        (setf ,grow (/ (- ,end ,beg) ,dur)))
       (+seg-exp-func+
        (setf ,grow (expt (the non-negative-sample (/ ,end ,beg))
                          (/ 1.0d0 ,dur))))
       (+seg-sine-func+
        (let ((,w (/ pi ,dur)))
          (setf ,a2 (* (+ ,end ,beg) 0.5d0)
                ,b1 (* 2.0d0 (cos (the maybe-limited-sample ,w)))
                ,y1 (* (- ,end ,beg) 0.5d0)
                ,y2 (* ,y1 (sin (the maybe-limited-sample
                                  (- +half-pi+ ,w)))))))
       (+seg-welch-func+
        (let ((,w (/ +half-pi+ ,dur)))
          (setf ,b1 (* 2.0d0 (cos (the maybe-limited-sample ,w))))
          (if (>= ,end ,beg)
              (setf ,a2 ,beg
                    ,y1 +sample-zero+
                    ,y2 (* (- (sin (the maybe-limited-sample ,w)))
                           (- ,end ,beg)))
              (setf ,a2 ,end
                    ,y1 (- ,beg ,end)
                    ,y2 (* (cos (the maybe-limited-sample ,w))
                           (- ,beg ,end))))))
       (+seg-square-func+
        (setf ,y1 (the non-negative-sample (sqrt ,beg))
              ,y2 (the non-negative-sample (sqrt ,end))
              ,grow (/ (- ,y2 ,y1) ,dur)))
       (+seg-cubic-func+
        (setf ,y1 (expt (the non-negative-sample ,beg) 0.3333333333333333d0)
              ,y2 (expt (the non-negative-sample ,end) 0.3333333333333333d0)
              ,grow (/ (- ,y2 ,y1) ,dur)))
       ;; custom curve
       (otherwise (if (< (abs ,curve) 0.001d0)
                      (setf ,grow (/ (- ,end ,beg) ,dur)
                            ,curve +seg-lin-func+)
                      (setf ,b1 (/ (- ,end ,beg) (- 1.0d0 (exp ,curve)))
                            ,a2 (+ ,beg ,b1)
                            ,grow (exp (/ ,curve ,dur))))))))

(defmacro %segment-init (beg end dur curve grow a2 b1 y1 y2)
  (let ((result `(%%segment-init ,beg ,end ,dur ,curve ,grow
                                 ,a2 ,b1 ,y1 ,y2)))
    #+double-samples result
    #-double-samples (apply-sample-coerce (macroexpand-1 result))))

(defmacro %segment-update-level (level curve grow a2 b1 y1 y2)
  (with-gensyms (y0)
    `(curve-case ,curve
       (+seg-step-func+ ,level)
       (+seg-lin-func+
        (setf ,level (+ ,level ,grow)))
       (+seg-exp-func+
        (setf ,level (* ,level ,grow)))
       (+seg-sine-func+
        (let ((,y0 (- (* ,b1 ,y1) ,y2)))
          (setf ,level (- ,a2 ,y0)
                ,y2 ,y1 ,y1 ,y0)))
       (+seg-welch-func+
        (let ((,y0 (- (* ,b1 ,y1) ,y2)))
          (setf ,level (+ ,a2 ,y0)
                ,y2 ,y1 ,y1 ,y0)))
       (+seg-square-func+
        (setf ,y1    (+ ,y1 ,grow)
              ,level (* ,y1 ,y1)))
       (+seg-cubic-func+
        (setf ,y1    (+ ,y1 ,grow)
              ,level (* ,y1 ,y1 ,y1)))
       ;; custom curve
       (otherwise (setf ,b1 (* ,b1 ,grow)
                        ,level (- ,a2 ,b1))))))

(defun make-envelope (levels times &key curve (loop-node -1)
                      (release-node -1) real-time-p)
  (declare (type list levels times)
           (type fixnum loop-node release-node)
           (type boolean real-time-p))
  (let* ((size (max (length levels) (1+ (length times))))
         (max-points (max size *envelope-default-max-points*))
         (max-data-size (calc-data-size max-points))
         (data (if real-time-p
                   (foreign-rt-alloc 'sample :count max-data-size)
                   (foreign-alloc-sample max-data-size)))
         (free-function (if real-time-p
                            #'foreign-rt-free
                            #'foreign-free))
         (env (%make-envelope :data data :data-size (calc-data-size size)
                              :points size :max-points max-points
                              :real-time-p real-time-p
                              :foreign-free free-function)))
    (set-envelope env levels times :curve curve :loop-node loop-node
                  :release-node release-node)
    (tg:finalize env (lambda ()
                       (rt-eval-if (real-time-p)
                         (funcall free-function data))))
    env))

(declaim (inline envelope-level))
(defun envelope-level (env node-number)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           #.*reduce-warnings*)
  (when (check-envelope-node env node-number)
    (smp-ref (envelope-data env)
             (if (zerop node-number)
                 0
                 (the non-negative-fixnum
                   (1- (* node-number 3)))))))

(declaim (inline check-envelope-node))
(defun check-envelope-node (env number)
  (declare (type non-negative-fixnum number))
  (and (>= number 0)
       (< number (the non-negative-fixnum
                   (envelope-points env)))))

(defun set-envelope-level (env node-number level)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           (type number level)
           #.*reduce-warnings*)
  (when (check-envelope-node env node-number)
    (let ((data (inc-pointer (envelope-data env)
                             (the non-negative-fixnum
                               (* node-number 3
                                  +foreign-sample-size+)))))
      (when (zerop level)
        (block nil
          (when (and (plusp node-number)
                     (exponential-curve-index-p
                      (smp-ref data 0)))
            (return (setf level (sample 1.0e-5))))
          (when (/= node-number (1- (the non-negative-fixnum
                                      (envelope-points env))))
            (incf-pointer data (* 3 +foreign-sample-size+))
            (when (exponential-curve-index-p
                   (smp-ref data 0))
              (incf-pointer data (* -3 +foreign-sample-size+))
              (return (setf level (sample 1.0e-5))))
            (incf-pointer data (* -3 +foreign-sample-size+)))))
      (if (> node-number 0)
          (incf-pointer data (- +foreign-sample-size+)))
      (setf (smp-ref data 0) (sample level)))))

(defsetf envelope-level set-envelope-level)

(declaim (inline envelope-time))
(defun envelope-time (env node-number)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           #.*reduce-warnings*)
  (when (check-envelope-node env node-number)
    (if (zerop node-number)
        +sample-zero+
        (smp-ref (envelope-data env)
                 (the non-negative-fixnum
                   (- (* node-number 3) 2))))))

(declaim (inline set-envelope-time))
(defun set-envelope-time (env node-number time)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           (type number time)
           #.*reduce-warnings*)
  (cond ((zerop node-number) +sample-zero+)
        ((< node-number (the non-negative-fixnum (envelope-points env)))
         (setf (smp-ref (envelope-data env)
                        (the positive-fixnum
                          (- (* node-number 3) 2)))
               (sample time)))
        (t +sample-zero+)))

(defsetf envelope-time set-envelope-time)

(declaim (inline envelope-curve))
(defun envelope-curve (env node-number)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           #.*reduce-warnings*
           #+(or cmu sbcl) (values (or symbol sample)))
  (when (< 0 node-number (the non-negative-fixnum
                           (envelope-points env)))
    (sample->seg-function-spec
     (smp-ref (envelope-data env)
              (the non-negative-fixnum
                (* 3 node-number))))))

(defun set-envelope-curve (env node-number curve)
  (declare #.*standard-optimize-settings*
           (type envelope env)
           (type non-negative-fixnum node-number)
           (type (or symbol real) curve)
           #.*reduce-warnings*
           #+(or cmu sbcl) (values (or symbol sample)))
  (macrolet ((segment-fix-zero (ptr new-zero old-zero)
               `(when (= (smp-ref ,ptr 0) ,old-zero)
                  (setf (smp-ref ,ptr 0) ,new-zero)))
             (segment-fix-zeros (beg-ptr end-ptr new-zero old-zero)
               `(progn
                  (segment-fix-zero ,beg-ptr ,new-zero ,old-zero)
                  (segment-fix-zero ,end-ptr ,new-zero ,old-zero))))
    (when (< 0 node-number (the non-negative-fixnum
                             (envelope-points env)))
      (let* ((curve-ptr (inc-pointer (envelope-data env)
                                     (the non-negative-fixnum
                                       (* node-number 3 +foreign-sample-size+))))
             (end-ptr (inc-pointer curve-ptr (- +foreign-sample-size+)))
             (beg-ptr (if (= node-number 1)
                          (envelope-data env)
                          (inc-pointer end-ptr (* -3 +foreign-sample-size+)))))
        (if (exponential-curve-p curve)
            (segment-fix-zeros beg-ptr end-ptr (sample 1.0e-5) 0)
            (segment-fix-zeros beg-ptr end-ptr +sample-zero+
                               (sample 1.0e-5)))
        (setf (smp-ref curve-ptr 0)
              (seg-function-spec->sample curve))
        curve))))

(defsetf envelope-curve set-envelope-curve)

(defmethod data ((obj envelope))
  (envelope-data obj))

(defmethod scale ((obj envelope) (mult real))
  (let ((points (envelope-points obj))
        (data (envelope-data obj)))
    (setf (envelope-level obj 0)
          (* (smp-ref data 0) mult))
    (loop for i from 1 below points do
         (setf (envelope-level obj i)
               (* (smp-ref data (1- (* i 3))) mult)))
    obj))

(defmethod normalize ((obj envelope) (norm-value real))
  (let ((points (envelope-points obj)))
    (declare (type positive-fixnum points))
    (labels ((norm (index maxval)
               (declare (type non-negative-fixnum index)
                        (type sample maxval))
               (if (= index points)
                   maxval
                   (norm (1+ index)
                         (max (envelope-level obj index) maxval)))))
      (let ((lev (envelope-level obj 0)))
        (if lev
            (scale obj (/ norm-value (norm 1 lev)))
            obj)))))

(defmethod rescale ((obj envelope) (min real) (max real))
  (let ((points (envelope-points obj)))
    (declare (type non-negative-fixnum points))
    (labels ((resc (index old-min old-max)
               (declare (type non-negative-fixnum index)
                        (type sample old-min old-max))
               (if (>= index points)
                   (values (/ (sample 1) (- old-max old-min))
                           old-min)
                   (let ((value (smp-ref (envelope-data obj) index)))
                     (resc (+ index 3)
                           (min value old-min)
                           (max value old-max))))))
      (multiple-value-bind (old-delta old-min)
          (let ((init (envelope-level obj 0)))
            (resc 2 init init))
        (let ((new-delta (- max min)))
          (flet ((set-level (index offset)
                   (setf (envelope-level obj index)
                         (+ min (* new-delta old-delta
                                   (- (smp-ref (envelope-data obj)
                                               offset)
                                      old-min))))))
            (set-level 0 0)
            (labels ((set-levels (index offset)
                       (declare (type non-negative-fixnum index offset))
                       (cond ((>= index points) obj)
                             (t (set-level index offset)
                                (set-levels (1+ index) (+ offset 3))))))
              (set-levels 1 2))))))))

(declaim (inline breakpoint-sequence-p))
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
    (if (and (consp curve)
             (/= (length curve) points))
        (complete-curve-list curve curve nil points)
        curve)))

(declaim (inline expanded-atomic-curve-plus-linear))
(defun expanded-atomic-curve-plus-linear (curve points)
  (do ((i 0 (1+ i))
       (acc nil))
      ((>= i points) (nreverse (cons :lin acc)))
    (push curve acc)))

(defun breakpoints->env (bpseq &key curve (loop-node -1)
                         (release-node -1) real-time-p)
  (declare (type (or list array) bpseq))
  (multiple-value-bind (bpseq-p size)
      (breakpoint-sequence-p bpseq)
    (if bpseq-p
        (labels ((rec (lst levels times old-time)
                   (if lst
                       (rec (cddr lst) (cons (cadr lst) levels)
                            (cons (- (car lst) old-time) times)
                            (car lst))
                       (make-envelope (nreverse levels) (nreverse times)
                                      :curve curve
                                      :loop-node loop-node
                                      :release-node release-node
                                      :real-time-p real-time-p))))
          (let ((bplist (coerce bpseq 'list)))
            (declare #.*standard-optimize-settings* #.*reduce-warnings*)
            (unless (zerop (car bplist))
              ;; Add the first pair
              (setf bplist (cons 0 (cons (cadr bplist) bplist)))
              (if curve
                  (let ((points (1- (the positive-fixnum (/ size 2)))))
                    (declare (type positive-fixnum points))
                    ;; Add the first curve
                    (setf curve (if (consp curve)
                                    (cons :lin (expand-curve-list curve points))
                                    (cons :lin (loop repeat points collect curve)))))))
            (rec (cddr bplist) (list (cadr bplist)) nil (car bplist))))
        (msg error "wrong breakpoint sequence"))))

(defun freq-breakpoints->env (bpseq &key (freq-max (* *sample-rate* 0.5))
                              curve real-time-p)
  (declare (type (or list array) bpseq))
  (multiple-value-bind (bpseq-p size)
      (breakpoint-sequence-p bpseq)
    (if bpseq-p
        (multiple-value-bind (bplist last-freq last-value)
            (if (listp bpseq)
                (let ((bpseq-rev (reverse bpseq)))
                  (values bpseq (cadr bpseq-rev) (car bpseq-rev)))
                (values (coerce bpseq 'list)
                        (aref bpseq (- size 2))
                        (aref bpseq (- size 1))))
          (let* ((points (1- (/ size 2)))
                 (curve (expand-curve-list curve points)))
            (unless (= last-freq freq-max)
              ;; Add the last pair
              (setf bplist (nconc bplist (list freq-max last-value)))
              (if curve
                  (setf curve (if (consp curve)
                                  (nconc (expand-curve-list curve points)
                                         (list :lin))
                                  (expanded-atomic-curve-plus-linear curve points)))))
            (breakpoints->env bplist :curve curve :real-time-p real-time-p)))
        (msg error "wrong breakpoint sequence"))))

;;; Frequently used envelope shapes

(defun make-linen (attack-time sustain-time release-time
                   &key (level 1) (curve :lin) real-time-p)
  (make-envelope (list 0 level level 0)
                 (list attack-time sustain-time release-time)
                 :curve curve :real-time-p real-time-p))

(defgeneric linen (obj attack-time sustain-time release-time
                   &key level curve))

(defmethod linen (obj attack-time sustain-time release-time
                  &key (level 1) (curve :lin))
  (set-envelope obj `(0 ,level ,level 0)
                `(,attack-time ,sustain-time ,release-time)
                :curve curve))

(defun make-perc (attack-time release-time
                  &key (level 1) (curve -4) real-time-p)
  (make-envelope (list 0 level 0) (list attack-time release-time)
                 :curve curve :real-time-p real-time-p))

(defgeneric perc (obj attack-time release-time &key level curve))

(defmethod perc (obj attack-time release-time
                 &key (level 1) (curve -4))
  (set-envelope obj `(0 ,level 0) `(,attack-time ,release-time)
                :curve curve))

(defun make-cutoff (release-time &key (level 1) (curve :exp) real-time-p)
  (make-envelope (list level 0) (list release-time)
                 :curve curve :release-node 0 :real-time-p real-time-p))

(defgeneric cutoff (obj release-time &key level curve))

(defmethod cutoff ((obj envelope) release-time &key (level 1)
                   (curve :exp))
  (set-envelope obj `(,level 0) `(,release-time)
                :curve curve :release-node 0))

(defun make-asr (attack-time sustain-level release-time
                 &key (curve -4) real-time-p)
  (make-envelope (list 0 sustain-level 0) (list attack-time release-time)
                 :curve curve :release-node 1 :real-time-p real-time-p))

(defgeneric asr (obj attack-time sustain-level release-time
                 &key curve))

(defmethod asr ((obj envelope) attack-time sustain-level release-time
                &key (curve -4))
  (set-envelope obj `(0 ,sustain-level 0) `(,attack-time ,release-time)
                :curve curve :release-node 1))

(defun make-adsr (attack-time decay-time sustain-level release-time
                  &key (peak-level 1) (curve -4) real-time-p)
  (make-envelope (list 0 peak-level (* peak-level sustain-level) 0)
                 (list attack-time decay-time release-time)
                 :curve curve :release-node 2 :real-time-p real-time-p))

(defgeneric adsr (obj attack-time decay-time sustain-level
                  release-time &key peak-level curve))

(defmethod adsr ((obj envelope) attack-time decay-time sustain-level
                 release-time &key (peak-level 1) (curve -4))
  (set-envelope obj `(0 ,peak-level ,(* peak-level sustain-level) 0)
                `(,attack-time ,decay-time ,release-time)
                :curve curve :release-node 2))

(defun make-dadsr (delay-time attack-time decay-time sustain-level
                   release-time &key (peak-level 1) (curve -4)
                   real-time-p)
  (make-envelope (list 0 0 peak-level (* peak-level sustain-level) 0)
                 (list delay-time attack-time decay-time release-time)
                 :curve curve :release-node 3 :real-time-p real-time-p))

(defgeneric dadsr (obj delay-time attack-time decay-time sustain-level
                   release-time &key peak-level curve))

(defmethod dadsr ((obj envelope) delay-time attack-time decay-time
                  sustain-level release-time &key (peak-level 1)
                  (curve -4))
  (set-envelope obj `(0 0 ,peak-level ,(* peak-level sustain-level) 0)
                `(,delay-time ,attack-time ,decay-time ,release-time)
                :curve curve :release-node 3))
