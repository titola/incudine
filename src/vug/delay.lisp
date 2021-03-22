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

(in-package :incudine.vug)

(defmacro with-ringbuffer-heads ((read-head write-head delay-samples mask)
                                 &body body)
  `(with ((,write-head 0)
          (,read-head (logand (- ,write-head ,delay-samples) ,mask)))
     (declare (type non-negative-fixnum ,read-head ,write-head))
     ,@body))

(defmacro update-ringbuffer (buffer input read-head write-head mask)
  `(progn
     (setf (smp-ref ,buffer ,write-head) ,input)
     (setf ,read-head (logand (1+ ,read-head) ,mask))
     (setf ,write-head (logand (1+ ,write-head) ,mask))))

(defmacro %delay-s (input delay-samples ringbuf mask)
  (with-gensyms (read-head write-head)
    `(with-ringbuffer-heads (,read-head ,write-head ,delay-samples ,mask)
       (prog1 (smp-ref ,ringbuf ,read-head)
         (update-ringbuffer ,ringbuf ,input ,read-head ,write-head ,mask)))))

(define-vug buf-delay-s ((buffer buffer) in
                         (delay-samples non-negative-fixnum))
  "Buffer based delay line with delay time in samples.

Use all the BUFFER memory if the BUFFER size is a power of two."
  (with ((ringbuf (buffer-data buffer))
         (mask (buffer-mask buffer)))
    (declare (type foreign-pointer ringbuf) (type non-negative-fixnum mask))
    (initialize (foreign-zero-sample ringbuf (1+ mask)))
    (%delay-s in delay-samples ringbuf mask)))

(define-vug buf-delay ((buffer buffer) in delay-time)
  "Buffer based delay line with DELAY-TIME in seconds.

Use all the BUFFER memory if the BUFFER size is a power of two."
  (buf-delay-s buffer in (sample->fixnum (* delay-time *sample-rate*))))

(define-vug delay-s (in (max-delay-samples positive-fixnum)
                     (delay-samples positive-fixnum))
  "Delay line with delay time in samples."
  (with ((size (next-power-of-two max-delay-samples))
         (ringbuf (make-frame size :zero-p t))
         (mask (1- size)))
    (declare (type frame ringbuf) (type non-negative-fixnum size mask))
    (%delay-s in delay-samples ringbuf mask)))

(define-vug delay (in max-delay-time delay-time)
  "Delay line with DELAY-TIME in seconds."
  (delay-s in (sample->fixnum (* max-delay-time *sample-rate*))
           (sample->fixnum (* delay-time *sample-rate*))))

(defmacro select-delay-interp (interp dsamps isamps data index mask)
  (with-gensyms (frac)
    (case interp
      (:linear `(with-samples ((,frac (- ,dsamps ,isamps)))
                  (linear-interp ,frac (smp-ref ,data ,index)
                                 (smp-ref ,data
                                          (logand (1- ,index) ,mask)))))
      (:cubic (with-gensyms (index0 index2 index3)
                `(with-samples ((,frac (- ,dsamps ,isamps)))
                   (let ((,index0 (logand (+ ,index 1) ,mask))
                         (,index2 (logand (- ,index 1) ,mask))
                         (,index3 (logand (- ,index 2) ,mask)))
                     (cubic-interp ,frac (smp-ref ,data ,index0)
                                   (smp-ref ,data ,index)
                                   (smp-ref ,data ,index2)
                                   (smp-ref ,data ,index3))))))
      ;; No interpolation
      (otherwise `(smp-ref ,data ,index)))))

(define-vug-macro vdelay (in max-delay-time delay-time
                          &optional (interpolation :linear))
  "Delay line with INTERPOLATION and DELAY-TIME in seconds.

INTERPOLATION is one of :LINEAR (default), :CUBIC or NIL."
  (with-gensyms (vdelay)
    `(vuglet ((,vdelay (in max-delay-time delay-time)
                (with ((size (next-power-of-two
                               (sample->fixnum (* max-delay-time
                                                  *sample-rate*))))
                       (mask (1- size))
                       (ringbuf (make-frame size :zero-p t))
                       (dsamps (* delay-time *sample-rate*))
                       (isamps (sample->fixnum dsamps))
                       (dt (clip isamps 0 mask)))
                  (declare (type frame ringbuf) (type sample dsamps)
                           (type non-negative-fixnum mask isamps dt))
                  (with-ringbuffer-heads (read-head write-head dt mask)
                    (prog1 (select-delay-interp ,interpolation dsamps isamps
                                                ringbuf read-head mask)
                      (update-ringbuffer ringbuf in read-head write-head
                                         mask))))))
       (,vdelay ,in ,max-delay-time ,delay-time))))

(define-vug-macro buf-vdelay (buffer in delay-time
                              &optional (interpolation :linear) write-head-var)
  "Buffer based delay line with INTERPOLATION and DELAY-TIME in seconds.

If WRITE-HEAD-VAR is non-NIL, it is the name of the variable used as
write head.

Use all the BUFFER memory if the BUFFER size is a power of two."
  (with-gensyms (buf-vdelay)
    `(vuglet ((,buf-vdelay ((buf buffer) in delay-time)
                (with ((dsamps (* delay-time *sample-rate*))
                       (isamps (sample->fixnum dsamps))
                       (mask (buffer-mask buf))
                       (dt (clip isamps 0 mask))
                       (data (buffer-data buf)))
                  (declare (type non-negative-fixnum isamps dt mask)
                           (type sample dsamps) (type foreign-pointer data))
                  (with-ringbuffer-heads (read-head write-head dt mask)
                    (initialize (foreign-zero-sample data (1+ mask)))
                    ,@(when write-head-var `((setq ,write-head-var write-head)))
                    (prog1 (select-delay-interp ,interpolation dsamps isamps
                                                data read-head mask)
                      (update-ringbuffer data in read-head write-head mask))))))
       (,buf-vdelay ,buffer ,in ,delay-time))))

(define-vug-macro vtap (buffer delay-time write-head-var
                        &optional interpolation)
  "Tap a buffer based delay line with INTERPOLATION at DELAY-TIME seconds.

WRITE-HEAD-VAR is the name of the variable used as write head.

Use all the BUFFER memory if the BUFFER size is a power of two."
  (with-gensyms (vtap read-head)
    `(vuglet ((,vtap ((buf buffer) delay-time)
                (with ((dsamps (* delay-time *sample-rate*))
                       (isamps (sample->fixnum dsamps))
                       (mask (buffer-mask buf))
                       (dt (clip isamps 0 mask))
                       (data (buffer-data buf)))
                  (declare (type non-negative-fixnum isamps dt mask)
                           (type sample dsamps) (type foreign-pointer data))
                  (let ((,read-head (logand (the fixnum (- ,write-head-var dt))
                                            mask)))
                    (select-delay-interp ,interpolation dsamps isamps data
                                         ,read-head mask)))))
       (,vtap ,buffer ,delay-time))))

(declaim (inline delay-feedback))
(defun delay-feedback (delay-time decay-time)
  "Feedback coefficient for a feed back comb filter with DELAY-TIME
and DECAY-TIME."
  (cond ((zerop decay-time) +sample-zero+)
        ((plusp decay-time)
         (exp (/ (* +log001+ delay-time) decay-time)))
        (t (- (exp (/ (* +log001+ delay-time) (- decay-time)))))))

(define-vug-macro ff-comb (in max-delay-time delay-time b0 bM
                           &optional (interpolation :linear))
  "Feed forward comb filter with DELAY-TIME in seconds."
  (with-gensyms (ff-comb)
    `(vuglet ((,ff-comb (in max-delay-time delay-time b0 bM)
                (with-samples ((in0 in)
                               (in1 (* b0 in0)))
                  (+ in1 (* bM (vdelay in0 max-delay-time delay-time
                                       ,interpolation))))))
       (,ff-comb ,in ,max-delay-time ,delay-time ,b0 ,bM))))

(define-vug-macro fb-comb (in max-delay-time delay-time coef
                           &optional (interpolation :linear))
  "Feed back comb filter with DELAY-TIME in seconds."
  (let ((it (ensure-symbol 'it))
        (fb-comb (gensym "FB-COMB")))
    `(vuglet ((,fb-comb (in max-delay-time delay-time coef)
                (~ (- in (* coef (vdelay ,it max-delay-time delay-time
                                         ,interpolation))))))
       (,fb-comb ,in ,max-delay-time ,delay-time ,coef))))

(define-vug allpass-s
    (in (max-delay-time positive-fixnum) (delay-time positive-fixnum) gain)
  "Allpass filter without interpolation and DELAY-TIME in samples."
  (with-samples (x y)
    (setf x (+ in (* gain y)))
    (prog1 (- y (* gain x))
      (setf y (delay-s x max-delay-time (1- delay-time))))))

(define-vug allpass (in max-delay-time delay-time gain)
  "Allpass filter without interpolation and DELAY-TIME in seconds."
  (allpass-s in
    ;; Rounding to the nearest integer works better than truncation
    ;; when the delay time is (* samples *sample-duration*)
    (sample->fixnum (* max-delay-time *sample-rate*) :roundp t)
    (sample->fixnum (* delay-time *sample-rate*) :roundp t) gain))

(define-vug-macro vallpass (in max-delay-time delay-time gain
                            &optional (interpolation :linear))
  "Allpass filter with DELAY-TIME in seconds."
  (with-gensyms (vallpass %in %gain)
    `(vuglet ((,vallpass (in max-delay-time delay-time gain)
                (with-samples (y dx)
                  (let ((,%in in) (,%gain gain))
                    (prog1 (setf y (+ (* (- ,%gain) ,%in) dx))
                      (setf dx (vdelay (+ ,%in (* ,%gain y)) max-delay-time
                                       delay-time ,interpolation)))))))
       (,vallpass ,in ,max-delay-time ,delay-time ,gain))))
