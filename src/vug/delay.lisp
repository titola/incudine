;;; Copyright (c) 2013-2015 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
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
    "Delay line with INTERPOLATION and DELAY-TIME in seconds."
    (with-gensyms (size dsamps isamps mask dt ringbuf read-head write-head)
      `(with ((,size (next-power-of-two (sample->fixnum (* ,max-delay-time
                                                           *sample-rate*))))
              (,mask (1- ,size))
              (,ringbuf (make-frame ,size :zero-p t))
              (,dsamps (vug-input (* ,delay-time *sample-rate*)))
              (,isamps (sample->fixnum ,dsamps))
              (,dt (clip ,isamps 0 ,mask)))
         (declare (type frame ,ringbuf) (type sample ,dsamps)
                  (type non-negative-fixnum ,mask ,isamps ,dt))
         (with-ringbuffer-heads (,read-head ,write-head ,dt ,mask)
           (prog1 (select-delay-interp ,interpolation ,dsamps ,isamps ,ringbuf
                                       ,read-head ,mask)
             (update-ringbuffer ,ringbuf ,in ,read-head ,write-head ,mask)))))))

(define-vug-macro buf-vdelay (buffer in delay-time
                              &optional (interpolation :linear) write-head-var)
  "Buffer based delay line with INTERPOLATION and DELAY-TIME in seconds.
If WRITE-HEAD-VAR is not NIL, it is the name of the variable used as
write head.
Use all the BUFFER memory if the BUFFER size is a power of two."
  (with-gensyms (%buffer dsamps isamps mask dt data write-head read-head)
    (with-coerce-arguments (in)
      `(with ((,%buffer (vug-input ,buffer))
              (,dsamps (vug-input (* ,delay-time *sample-rate*)))
              (,isamps (sample->fixnum ,dsamps))
              (,mask (buffer-mask ,%buffer))
              (,dt (clip ,isamps 0 ,mask))
              (,data (buffer-data ,%buffer)))
         (declare (type buffer ,%buffer)
                  (type non-negative-fixnum ,isamps ,dt ,mask)
                  (type sample ,dsamps) (type foreign-pointer ,data))
         (with-ringbuffer-heads (,read-head ,write-head ,dt ,mask)
           (initialize (foreign-zero-sample ,data (1+ ,mask)))
           ,@(when write-head-var `((setq ,write-head-var ,write-head)))
           (prog1 (select-delay-interp ,interpolation ,dsamps ,isamps ,data
                                       ,read-head ,mask)
             (update-ringbuffer ,data ,in ,read-head ,write-head ,mask)))))))

(define-vug-macro vtap (buffer delay-time write-head-var
                        &optional interpolation)
  "Taps a buffer based delay line with INTERPOLATION at DELAY-TIME seconds.
WRITE-HEAD-VAR is the name of the variable used as write head.
Use all the BUFFER memory if the BUFFER size is a power of two."
  (with-gensyms (%buffer dsamps isamps mask dt data read-head)
    `(with ((,%buffer (vug-input ,buffer))
            (,dsamps (vug-input (* ,delay-time *sample-rate*)))
            (,isamps (sample->fixnum ,dsamps))
            (,mask (buffer-mask ,%buffer))
            (,dt (clip ,isamps 0 ,mask))
            (,data (buffer-data ,%buffer)))
       (declare (type buffer ,%buffer)
                (type non-negative-fixnum ,isamps ,dt ,mask)
                (type sample ,dsamps) (type foreign-pointer ,data))
       (let ((,read-head (logand (the fixnum (- ,write-head-var ,dt)) ,mask)))
         (select-delay-interp ,interpolation ,dsamps ,isamps ,data ,read-head
                              ,mask)))))

(declaim (inline delay-feedback))
(defun delay-feedback (delay-time decay-time)
  (cond ((zerop decay-time) +sample-zero+)
        ((plusp decay-time)
         (exp (/ (* +log001+ delay-time) decay-time)))
        (t (- (exp (/ (* +log001+ delay-time) (- decay-time)))))))

(define-vug-macro ff-comb (in max-delay-time delay-time b0 bM
                           &optional (interpolation :linear))
  "Feed forward comb filter with DELAY-TIME in seconds."
  (with-gensyms (in0 in1)
    (with-coerce-arguments (b0 bM)
      `(with-vug-inputs ((,in0 ,in)
                         (,in1 (* ,b0 ,in0)))
         (declare (type sample ,in0 ,in1))
         (+ ,in1
            (* ,bM (vdelay ,in0 ,max-delay-time ,delay-time
                           ,interpolation)))))))

(define-vug-macro fb-comb (in max-delay-time delay-time coef
                           &optional (interpolation :linear))
  "Feed back comb filter with DELAY-TIME in seconds."
  (let ((it (ensure-symbol 'it)))
    `(~ (- ,in (* ,coef (vdelay ,it ,max-delay-time ,delay-time
                                ,interpolation))))))

(define-vug allpass (in max-delay-time delay-time gain)
  "Allpass filter without interpolation and DELAY-TIME in seconds."
  (with-samples (y dx)
    (prog1 y
      (setf y (+ (* (- gain) in) dx))
      (setf dx (delay (+ in (* gain y)) max-delay-time delay-time)))))

(define-vug-macro vallpass (in max-delay-time delay-time gain
                            &optional (interpolation :linear))
  "Allpass filter with DELAY-TIME in seconds."
  (with-gensyms (y dx %in %gain)
    (with-coerce-arguments (gain)
      `(with-samples (,y ,dx)
         (let ((,%in ,in) (,%gain ,gain))
           (prog1 (setf ,y (+ (* (- ,%gain) ,%in) ,dx))
             (setf ,dx (vdelay (+ ,%in (* ,%gain ,y)) ,max-delay-time
                               ,delay-time ,interpolation))))))))
