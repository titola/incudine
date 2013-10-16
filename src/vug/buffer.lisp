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

(defmacro make-local-buffer (&whole whole frames &key (channels 1) file offset
                             sample-rate initial-contents fill-function)
  (declare (ignore channels file offset sample-rate initial-contents
                   fill-function))
  (with-gensyms (buf)
    `(with ((,buf (incudine:make-buffer ,frames :real-time-p t
                                        ,@(cddr whole))))
       (declare (type buffer ,buf))
       ,buf)))

(defmacro update-local-buffer (vug-varname args)
  (with-gensyms (frames channels)
    `(let ((,frames ,(car args))
           (,channels ,(or (getf (cdr args) :channels) 1)))
       (declare (type non-negative-fixnum ,frames ,channels))
       (cond ((> (* ,frames ,channels) #1=(buffer-size ,vug-varname))
              (foreign-rt-free #2=(buffer-data ,vug-varname))
              (setf ,vug-varname (incudine:make-buffer ,frames ,@(cdr args))))
             (t ,(unless (do ((pl (cdr args) (cddr pl)))
                             ((or (null pl)
                                  (member (car pl) '(:initial-contents
                                                     :fill-function)))
                              pl))
                   `(foreign-zero-sample #2# #1#)))))))

(defmacro make-local-abuffer (analysis-object)
  (with-gensyms (abuf)
    `(with ((,abuf (incudine.analysis:make-abuffer ,analysis-object t)))
       ,abuf)))

(defmacro update-local-abuffer (vug-varname args)
  (with-gensyms (obj)
    `(let ((,obj ,(car args)))
       (cond ((eq ,obj (incudine.analysis::abuffer-link ,vug-varname))
              (setf (incudine.analysis:abuffer-time ,vug-varname) (sample -1))
              (foreign-zero-sample (incudine.analysis::abuffer-data ,vug-varname)
                                   (incudine.analysis::abuffer-size ,vug-varname)))
             (t (incudine:free ,vug-varname)
                (setf ,vug-varname (incudine.analysis:make-abuffer ,obj t)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (object-to-free incudine:make-buffer update-local-buffer)
  (object-to-free incudine.analysis:make-abuffer update-local-abuffer)

  (defmacro %check-phase (phase function)
    `(progn
       ;; Possible expansion of PHASE before to call FUNCTION
       ,phase
       (funcall ,function)))

  (defmacro %no-interp (data phs channels wrap-phase-fn)
    (with-gensyms (index)
      `(with ((,index 0))
         (declare (type non-negative-fixnum ,index))
         (foreach-tick
           (%check-phase ,phs ,wrap-phase-fn)
           (setf ,index (the non-negative-fixnum
                          (* (sample->fixnum ,phs) ,channels))))
         (if (< current-channel ,channels)
             (smp-ref ,data (the non-negative-fixnum
                              (+ ,index current-channel)))
             +sample-zero+))))

  (defmacro %two-points-interp (data phs frames channels size wrap-p
                                wrap-phase-fn interp-fn-name)
    (with-gensyms (frac iphase guard-frame decr y0 y1)
      `(with ((,iphase 0)
              (,frac 0.0d0)
              (,guard-frame (- ,frames 2))
              (,decr (- (if ,wrap-p ,size ,channels)))
              (,y0 0)
              (,y1 0))
         (declare (type fixnum ,iphase ,y0 ,y1)
                  (type sample ,frac) (type positive-fixnum ,guard-frame)
                  (type negative-fixnum ,decr))
         (foreach-tick
           (%check-phase ,phs ,wrap-phase-fn)
           (setf ,iphase (sample->fixnum ,phs))
           (setf ,frac (- ,phs ,iphase))
           (setf ,y0 (the fixnum (* ,iphase ,channels))
                 ,y1 (the fixnum (+ ,y0 ,channels)))
           (when (> ,iphase ,guard-frame)
             (incf ,y1 ,decr)))
         (if (< current-channel ,channels)
             (,interp-fn-name ,frac
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y0 current-channel)))
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y1 current-channel))))
             +sample-zero+))))

  (defmacro %four-points-interp (data phs frames channels size
                                 wrap-p wrap-phase-fn interp-fn-name)
    (with-gensyms (frac iphase guard-frame incr1 incr2
                   decr1 decr2 y0 y1 y2 y3)
      `(with ((,iphase 0)
              (,frac 0.0d0)
              (,guard-frame (- ,frames 2))
              (,incr1 (if ,wrap-p ,size ,channels))
              (,incr2 (if ,wrap-p ,size (* 2 ,channels)))
              (,decr1 (- ,incr1))
              (,decr2 (- ,incr2))
              (,y0 0)
              (,y1 0)
              (,y2 0)
              (,y3 0))
         (declare (type fixnum ,iphase ,y0 ,y1 ,y2 ,y3)
                  (type sample ,frac)
                  (type positive-fixnum ,guard-frame ,incr1 ,incr2)
                  (type negative-fixnum ,decr1 ,decr2))
         (foreach-tick
           (%check-phase ,phs ,wrap-phase-fn)
           (setf ,iphase (sample->fixnum ,phs))
           (setf ,frac (- ,phs ,iphase))
           (setf ,y1 (the fixnum (* ,iphase ,channels))
                 ,y0 (the fixnum (- ,y1 ,channels))
                 ,y2 (the fixnum (+ ,y1 ,channels))
                 ,y3 (the fixnum (+ ,y2 ,channels)))
           (cond ((zerop ,iphase)
                  (incf ,y0 ,incr1))
                 ((= ,iphase ,guard-frame)
                  (incf ,y3 ,decr1))
                 ((> ,iphase ,guard-frame)
                  (incf ,y2 ,decr1)
                  (incf ,y3 ,decr2))))
         (if (< current-channel ,channels)
             (,interp-fn-name ,frac
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y0 current-channel)))
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y1 current-channel)))
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y2 current-channel)))
                              (smp-ref ,data (the non-negative-fixnum
                                               (+ ,y3 current-channel))))
             +sample-zero+))))

  (defmacro wrap-phase-func (phs frames wrap-p)
    (with-gensyms (max)
      `(with-samples ((,max (sample (- ,frames (if ,wrap-p 0 1)))))
         (if ,wrap-p
             (lambda ()
               (wrap-cond (,phs +sample-zero+ ,max ,max)
                 ((>= ,phs ,max) (decf ,phs ,max))
                 ((minusp ,phs)  (incf ,phs ,max)))
               (values))
             (lambda ()
               (cond ((>= ,phs ,max)
                      (setf (done-self) t ,phs ,max))
                     ((minusp ,phs)
                      (setf (done-self) t ,phs +sample-zero+)))
               (values))))))

  (defmacro select-buffer-interp (interp data phs frames channels size
                                  wrap-p wrap-phase-fn)
    (case interp
      (:linear
       `(%two-points-interp ,data ,phs ,frames ,channels ,size
                            ,wrap-p ,wrap-phase-fn linear-interp))
      (:cubic
       `(%four-points-interp ,data ,phs ,frames ,channels ,size
                             ,wrap-p ,wrap-phase-fn cubic-interp))
      (otherwise `(%no-interp ,data ,phs ,channels ,wrap-phase-fn)))))

(define-vug-macro buffer-read (buffer phase &key wrap-p interpolation)
  (with-gensyms (%buffer %phs %wrap-p frames channels data wrap-phase-fn)
    `(with-vug-inputs ((,%buffer ,buffer)
                       (,%phs (sample ,phase))
                       (,%wrap-p ,wrap-p))
       (declare (type buffer ,%buffer) (type sample ,%phs)
                (type boolean ,%wrap-p))
       (with ((,frames (buffer-frames ,%buffer))
              (,channels (buffer-channels ,%buffer))
              (,data (buffer-data ,%buffer))
              (,wrap-phase-fn (wrap-phase-func ,%phs ,frames ,%wrap-p)))
         (declare (type non-negative-fixnum ,frames ,channels)
                  (type function ,wrap-phase-fn))
         (select-buffer-interp ,interpolation ,data ,%phs ,frames ,channels
                               (buffer-size ,%buffer) ,%wrap-p ,wrap-phase-fn)))))

;;; Write an input to a buffer at an index.
;;; If INDEX-VAR is the name of an existent variable, the value of the
;;; index is stored in this variable.
(define-vug-macro buffer-write (buffer index input &optional index-var)
  (with-gensyms (%buffer %index data upper-limit)
    (with-coerce-arguments (input)
      `(with ((,%buffer (vug-input ,buffer))
              (,data (buffer-data ,%buffer))
              (,upper-limit (1- (buffer-size ,%buffer))))
         (declare (type buffer ,%buffer) (type foreign-pointer ,data)
                  (type non-negative-fixnum ,upper-limit))
         (let* ((,%index (clip ,index 0 ,upper-limit)))
           (declare (type fixnum ,%index))
           ,@(when index-var `((setq (external-variable ,index-var) ,%index)))
           (setf (smp-ref ,data ,%index) ,input))))))

(define-vug-macro buffer-frame (buffer phase &key wrap-p interpolation)
  (with-gensyms (%buffer channels frame)
    `(with ((,%buffer (vug-input ,buffer))
            (,channels (buffer-channels ,%buffer))
            (,frame (make-frame ,channels)))
       (dochannels (current-channel ,channels)
         (setf (frame-ref ,frame current-channel)
               (buffer-read ,%buffer ,phase
                            :wrap-p ,wrap-p
                            :interpolation ,interpolation)))
       ,frame)))
