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

(in-package :incudine.vug)

;;; Delay line without interpolation
(define-vug buf-delay-s ((buffer buffer) in
                         (delay-samples non-negative-fixnum))
  (with ((size (buffer-size buffer))
         (upper-limit (cond ((> delay-samples size) (1- size))
                            ((> delay-samples 0) (1- delay-samples))
                            (t 0)))
         (data (buffer-data buffer))
         (index 0))
    (declare (type non-negative-fixnum size upper-limit index)
             (type foreign-pointer data))
    (prog1 #1=(smp-ref data index)
      (setf #1# in)
      (setf index (the non-negative-fixnum
                    (if (>= index upper-limit) 0 (1+ index)))))))

;;; Delay line without interpolation (time in seconds)
(define-vug buf-delay ((buffer buffer) in delay-time)
  (buf-delay-s buffer in (sample->fixnum (* delay-time *sample-rate*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
        (otherwise `(smp-ref ,data ,index))))))

;;; Variable delay line with interpolation (time in seconds).
;;; If WR-INDEX-VAR is the name of an existent variable, the value of the
;;; write-index is stored in this variable. It is useful with VTAP.
(define-vug-macro buf-vdelay (buffer in delay-time
                              &optional (interpolation :linear) wr-index-var)
  (with-gensyms (%buffer dsamps isamps mask dt data wr-index rd-index)
    (with-coerce-arguments (in)
      `(with ((,%buffer (vug-input ,buffer))
              (,dsamps (vug-input (* ,delay-time *sample-rate*)))
              (,isamps (sample->fixnum ,dsamps))
              (,mask (buffer-mask ,%buffer))
              (,dt (clip ,isamps 0 ,mask))
              (,data (buffer-data ,%buffer))
              (,wr-index 0))
         (declare (type buffer ,%buffer)
                  (type non-negative-fixnum ,isamps ,dt ,wr-index ,mask)
                  (type sample ,dsamps) (type foreign-pointer ,data))
         (let ((,rd-index (logand (the fixnum (- ,wr-index ,dt)) ,mask)))
           ,@(when wr-index-var `((setq (external-variable ,wr-index-var) ,wr-index)))
           (prog1
             (select-delay-interp ,interpolation ,dsamps ,isamps ,data ,rd-index ,mask)
             (setf (smp-ref ,data ,wr-index) ,in)
             (setf ,wr-index (the non-negative-fixnum
                               (if (>= ,wr-index ,mask) 0 (1+ ,wr-index))))))))))

(define-vug-macro vtap (buffer delay-time wr-index-var &optional interpolation)
  (with-gensyms (%buffer dsamps isamps mask dt data rd-index)
    `(with ((,%buffer (vug-input ,buffer))
            (,dsamps (vug-input (* ,delay-time *sample-rate*)))
            (,isamps (sample->fixnum ,dsamps))
            (,mask (buffer-mask ,%buffer))
            (,dt (clip ,isamps 0 ,mask))
            (,data (buffer-data ,%buffer)))
       (declare (type buffer ,%buffer)
                (type non-negative-fixnum ,isamps ,dt ,mask)
                (type sample ,dsamps) (type foreign-pointer ,data))
       (let ((,rd-index (logand (the fixnum
                                  (- (external-variable ,wr-index-var) ,dt))
                                ,mask)))
         (select-delay-interp ,interpolation ,dsamps ,isamps ,data ,rd-index ,mask)))))

;;; Delay line with local ring buffer and without interpolation
(define-vug delay-s (in (max-delay-samples non-negative-fixnum)
                        (delay-samples non-negative-fixnum))
  (with ((buf (make-local-buffer max-delay-samples)))
    (declare (type buffer buf))
    (buf-delay-s buf in delay-samples)))

;;; Delay line with local ring buffer and without interpolation
;;; (time in seconds)
(define-vug delay (in max-delay-time delay-time)
  (delay-s in (sample->fixnum (* max-delay-time *sample-rate*))
           (sample->fixnum (* delay-time *sample-rate*))))

;;; Variable delay line with local ring buffer and interpolation
;;; (time in seconds)
(define-vug-macro vdelay (in max-delay-time delay-time
                          &optional (interpolation :linear))
  (with-gensyms (buf)
    `(with ((,buf (make-local-buffer
                   (next-power-of-two
                    (sample->fixnum (* ,max-delay-time *sample-rate*))))))
       (declare (type buffer ,buf))
       (buf-vdelay ,buf ,in ,delay-time ,interpolation))))

(declaim (inline delay-feedback))
(defun delay-feedback (delay-time decay-time)
  (cond ((zerop decay-time) +sample-zero+)
        ((plusp decay-time)
         (exp (/ (* +log001+ delay-time) decay-time)))
        (t (- (exp (/ (* +log001+ delay-time) (- decay-time)))))))

;;; Feed Forward Comb Filter
(define-vug-macro ff-comb (in max-delay-time delay-time b0 bM
                           &optional (interpolation :linear))
  (with-gensyms (in0 in1)
    (with-coerce-arguments (b0 bM)
      `(with-vug-inputs ((,in0 ,in)
                         (,in1 (* ,b0 ,in0)))
         (declare (type sample ,in0 ,in1))
         (+ ,in1
            (* ,bM (vdelay ,in0 ,max-delay-time ,delay-time ,interpolation)))))))

;;; Feed Back Comb Filter
(define-vug-macro fb-comb (in max-delay-time delay-time coef
                           &optional (interpolation :linear))
  (with-gensyms (y)
    `(with-samples (,y)
       (setf ,y (- ,in (* ,coef (vdelay ,y ,max-delay-time
                                        ,delay-time ,interpolation)))))))

;;; Allpass filter without interpolation of the delay line
(define-vug allpass (in max-delay-time delay-time gain)
  (with-samples (y dx)
    (prog1 y
      (setf y (+ (* (- gain) in) dx))
      (setf dx (delay (+ in (* gain y))
                      max-delay-time delay-time)))))

;;; Allpass filter with interpolation of the delay line
(define-vug-macro vallpass (in max-delay-time delay-time gain
                            &optional (interpolation :linear))
  (with-gensyms (y dx %in %gain)
    (with-coerce-arguments (gain)
      `(with-samples (,y ,dx)
         (let ((,%in ,in) (,%gain ,gain))
           (prog1 (setf ,y (+ (* (- ,%gain) ,%in) ,dx))
             (setf ,dx (vdelay (+ ,%in (* ,%gain ,y)) ,max-delay-time ,delay-time
                               ,interpolation))))))))
