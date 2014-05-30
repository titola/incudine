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

(in-package :incudine.vug)

(defmacro done-action (action)
  `(funcall ,action (dsp-node)))

(defmacro done-self ()
  `(incudine::node-done-p (dsp-node)))

(defmacro free-self ()
  `(incudine:free (dsp-node)))

(defmacro free-self-when-done ()
  `(when (done-self) (free-self)))

;;; A FRAME is a foreign array of SAMPLE type, useful to efficiently
;;; store and return multiple values from a VUG
(defmacro make-frame (size &key zero-p initial-element initial-contents)
  (with-gensyms (frame-wrap)
    `(with ((,frame-wrap (make-foreign-array ,size 'sample
                           ,@(if zero-p `(:zero-p ,zero-p))
                           ,@(if initial-element
                                 `(:initial-element ,initial-element))
                           ,@(if initial-contents
                                 `(:initial-contents ,initial-contents)))))
       (declare (type foreign-array ,frame-wrap))
       (foreign-array-data ,frame-wrap))))

;;; Return a value of a frame
(defmacro frame-ref (frame channel)
  `(smp-ref ,frame ,channel))

;;; Like MULTIPLE-VALUE-BIND but dedicated to a FRAME
(defmacro multiple-sample-bind (vars frame &body body)
  (with-gensyms (frm)
    `(with ((,frm ,frame))
       (symbol-macrolet
           ,(loop for var in vars for count from 0
                  collect `(,var (frame-ref ,frm ,count)))
         ,@body))))

(defmacro samples (&rest values)
  (with-gensyms (frm)
    (let ((size (length values)))
    `(with ((,frm (make-frame ,size)))
       ,@(loop for value in values for count from 0
               collect `(setf (frame-ref ,frm ,count)
                              ,(if (and (numberp value)
                                        (not (typep value 'sample)))
                                   (sample value)
                                   value)))
       (values ,frm ,size)))))

;;; Calc only one time during a tick
(defmacro foreach-tick (&body body)
  (with-gensyms (old-time)
    `(with-samples ((,old-time -1.0d0))
       (unless (= (now) ,old-time)
         (setf ,old-time (now))
         ,@body)
       (values))))

(defmacro foreach-channel (&body body)
  (with-gensyms (i)
    `(dochannels (,i *number-of-output-bus-channels*)
       (let ((current-channel ,i))
         (declare (type channel-number current-channel)
                  (ignorable current-channel))
         ,@body))))

;;; Count from START to END (excluded)
(define-vug-macro counter (start end &key (step 1) loop-p done-action)
  (with-gensyms (%start %end %step index done-p %loop-p)
    `(with-vug-inputs ((,%start ,start)
                       (,%end ,end)
                       (,%step ,step)
                       (,%loop-p ,loop-p))
       (declare (type fixnum ,%start ,%end ,%step) (type boolean ,%loop-p))
       (with ((,done-p nil)
              (,index (progn (if ,done-p (setf ,done-p nil))
                             ,%start)))
         (declare (type fixnum ,index) (type boolean ,done-p))
         (prog1 ,index
           (unless ,done-p
             (setf ,index (the fixnum
                            (if (< ,index ,%end)
                                (+ ,index ,%step)
                                (if ,%loop-p
                                    ,%start
                                    (progn
                                      (done-action
                                       ,(or done-action '(function identity)))
                                      (setf ,done-p t)
                                      ,index)))))))))))

(define-vug downsamp ((control-period fixnum) in)
  (with ((count control-period)
         (value +sample-zero+))
    (declare (type fixnum count) (type sample value))
    (initialize (setf count 0))
    (if (<= count 1)
        (setf count control-period value in)
        (progn (decf count) value))))

;;; The input is valued every GATE samples, on demand or never.
;;; If GATE is positive, the output is the input calculated every GATE samples.
;;; If GATE is zero, the output is the old value of the input.
;;; If GATE is negative, the output is the current value of the input and GATE
;;; becomes zero.
(define-vug generic-rate ((gate fixnum) (start-offset fixnum) in)
  (with ((count (init-only gate))
         (value 0.0d0))
    (declare (type fixnum count) (type sample value))
    (initialize (setf count (1+ start-offset)))
    (cond ((plusp gate)
           (if (= count 1)
               (setf value (update in) count gate)
               (decf count)))
          ((minusp gate)
           (setf value (update in) gate 0)))
    value))

(define-vug samphold (in gate initial-value initial-threshold)
  (with-samples ((threshold initial-threshold)
                 (value initial-value))
    (when (< gate threshold)
      (setf value in))
    (setf threshold gate)
    value))

(define-vug lin->lin (in old-min old-max new-min new-max)
  (with-samples ((old-rdelta (/ (sample 1) (- old-max old-min)))
                 (new-delta (- new-max new-min)))
    (+ new-min (* new-delta old-rdelta (- in old-min)))))

(define-vug lin->exp (in old-min old-max new-min new-max)
  (with-samples ((old-rdelta (/ (sample 1) (- old-max old-min)))
                 (new-ratio (/ new-max new-min)))
    (* (expt (the non-negative-sample new-ratio)
             (* old-rdelta (- in old-min)))
       new-min)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %with-samples-rebinding (bindings &body body)
    `(let ,(mapcar (lambda (x)
                     (let ((value (cadr x)))
                       `(,(car x) (if (atom ,value) ,value (gensym)))))
                   bindings)
       `(with-samples (,@,@(mapcar (lambda (x)
                                     (let ((value (cadr x)))
                                       `(unless (atom ,value)
                                          `((,,(car x) (vug-input ,,value))))))
                                   bindings))
          ,,@body)))

  (define-vug-macro nclip (in low high)
    (%with-samples-rebinding ((lo low) (hi high))
      `(progn
         (cond ((> ,in ,hi) (setf ,in ,hi))
               ((< ,in ,lo) (setf ,in ,lo)))
         ,in)))

  (defmacro wrap-cond ((in low high range &optional (offset in))
                       &rest clauses)
    `(progn
       ;; If the input is to set, here is a good place, because the
       ;; first condition in the next COND contains an explicit setter
       ;; form, so the automatic setter is disabled.
       (maybe-expand ,in)
       (cond ((zerop ,range)
              (setf ,in ,(if (eql high range) +sample-zero+ low)))
             ,@(mapcar (lambda (x)
                         `(,(car x) ,@(cdr x)
                            (when ,(car x)
                              (decf ,in (* ,range (sample->fixnum
                                                   (/ ,offset ,range)))))))
                       clauses))))

  (define-vug-macro nwrap (in low high &optional range offset)
    (%with-samples-rebinding ((lo low) (hi high))
      (let ((delta (or range hi)))
        `(progn
           (wrap-cond (,in ,lo ,hi ,delta ,(or offset in))
             ((>= ,in ,hi) (decf ,in ,delta))
             (,(if range `(< ,in ,lo) `(minusp ,in))
              (incf ,in ,delta)))
           ,in))))

  (defmacro %mirror-consequent (in threshold1 threshold2 range
                                two-range offset offset-p bias)
    (with-gensyms (os)
      `(progn
         (setf ,in (- (+ ,threshold1 ,threshold1) ,in))
         (if (< ,in ,threshold2)
             (let ((,os ,offset))
               (setf ,in (- ,os (* ,two-range
                                   (sample->fixnum (/ ,os ,two-range)))))
               (when (>= ,in ,range)
                 (setf ,in (- ,two-range ,in)))
               ,(if offset-p `(+ ,in ,bias) in))
             ,in))))

  (define-vug-macro nmirror (in low high &optional range two-range offset)
    (%with-samples-rebinding ((lo low) (hi high))
      (let ((%range (or range hi))
            (%offset (or offset in)))
          `(progn
             (maybe-expand ,in)
             (cond ((zerop ,%range)
                    (setf ,in ,(if (eql hi %range) +sample-zero+ lo)))
                   ((>= ,in ,hi)
                    (%mirror-consequent ,in ,hi ,lo ,%range ,two-range
                                        ,%offset ,offset ,lo))
                   ((< ,in ,lo)
                    (%mirror-consequent ,in ,lo ,hi ,%range ,two-range
                                        ,%offset ,offset ,lo))
                   (t ,in)))))))

(declaim (inline clip))
(defun clip (in low high)
  (flet ((%clip (in low high)
           (cond ((> in high) high)
                 ((< in low) low)
                 (t in))))
    (if (typep in 'sample)
        (%clip in (sample low) (sample high))
        (%clip in low high))))

(define-vug wrap (in low high)
  (with-samples ((range (- high low))
                 (%in in))
    (nwrap %in low high range (- %in low))))

(define-vug mirror (in low high)
  (with-samples ((range (- high low))
                 (two-range (+ range range))
                 (%in in))
    (nmirror %in low high range two-range (- in low))))

;;; Interpolation of the values generated by a VUG. The values of the
;;; generator are calculated with a modulable frequency. The possible
;;; types of the interpolation are :LINEAR (or :LIN), :COS, :CUBIC or NIL.
;;; INTERPOLATE is particularly useful with the random or chaotic VUGs.
(define-vug-macro interpolate (generator freq
                               &optional (interpolation :linear)
                               initial-value-p)
  (with-gensyms (input phase inc x0 x1 x2 x3 delta)
    (destructuring-bind (bindings init update result)
        (case interpolation
          ((:lin :linear)
           `(((,x1 0.0d0) (,delta 0.0d0))
             (setf ,x1 ,input)
             (setf ,x0 ,x1 ,x1 (update ,input) ,delta (- ,x0 ,x1))
             (+ ,x1 (* ,phase ,delta))))
          (:cos `(((,x1 0.0d0))
                  (setf ,x1 ,input)
                  (setf ,x0 ,x1 ,x1 (update ,input))
                  (cos-interp ,phase ,x1 ,x0)))
          (:cubic `(((,x1 0.0d0) (,x2 0.0d0) (,x3 0.0d0))
                    (setf ,x1 ,input
                          ;; Three adjacent points initialized with the same
                          ;; value when it is required an initial value.
                          ,x2 ,(if initial-value-p input `(update ,input))
                          ,x3 ,(if initial-value-p input `(update ,input)))
                    (setf ,x0 ,x1 ,x1 ,x2 ,x2 ,x3 ,x3 (update ,input))
                    (cubic-interp ,phase ,x3 ,x2 ,x1 ,x0)))
          (otherwise `(nil nil (setf ,x0 (update ,input)) ,x0)))
      `(with-samples ((,input (vug-input ,generator))
                      (,phase 0.0d0)
                      (,inc (vug-input (* ,freq *sample-duration*)))
                      (,x0 0.0d0)
                      ,@bindings)
         ,@(when init `((initialize ,init)))
         (decf ,phase ,inc)
         (when (minusp ,phase)
           (setf ,phase (wrap ,phase 0 1))
           ,update)
         ,result))))
