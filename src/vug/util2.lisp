;;; Copyright (c) 2013-2019 Tito Latini
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

(defun free-self ()
  "Free the DSP node."
  (incudine:free (dsp-node)))

;;;  +--------------------------------+
;;;  |   Header of a foreign array    |
;;;  +--------+-----------------------+
;;;  | 4 bits |        24 bits        |
;;;  +--------+-----------------------+
;;;  |  type  |      array length     |
;;;  +--------+-----------------------+

(define-constant +foreign-header-size+ 4)

(define-constant +foreign-array-length-bits+ 24)

(define-constant +foreign-array-length-mask+ #xFFFFFF)

(define-constant +unknown-foreign-type+ 0)

(defvar *foreign-array-types*
  #(:unknown sample :int32 :uint32 :int64 :uint64 :float :double :pointer))
(declaim (type simple-vector *foreign-array-types*))

(declaim (inline foreign-type-to-tag))
(defun foreign-type-to-tag (type)
  (the (unsigned-byte 28)
       (ash (or (position type *foreign-array-types* :test #'eq)
                +unknown-foreign-type+)
            +foreign-array-length-bits+)))

(declaim (inline make-foreign-header))
(defun make-foreign-header (size type)
  (logior (foreign-type-to-tag type)
          (logand size +foreign-array-length-mask+)))

(defmacro %make-foreign-array (size type &rest args)
  (let ((arr-wrap (gensym (format nil "~A-WRAP" type)))
        (data (gensym "DATA")))
    `(with ((,arr-wrap (make-foreign-array (the positive-fixnum (1+ ,size))
                                           ',type ,@args))
            (,data (let ((,data (foreign-array-data ,arr-wrap)))
                     (setf (cffi:mem-ref ,data :uint32
                                         ,(- (cffi:foreign-type-size type)
                                             +foreign-header-size+))
                           (make-foreign-header ,size ',type))
                     (cffi:inc-pointer ,data ,(cffi:foreign-type-size type)))))
       (declare (type foreign-array ,arr-wrap) (type foreign-pointer ,data))
       ,data)))

(declaim (inline foreign-length))
(defun foreign-length (pointer)
  "Return the number of elements in a foreign array created by one of
the make-*-array utilities associated with a variable in WITH binding."
  (logand (cffi:mem-ref (cffi:inc-pointer pointer (- +foreign-header-size+))
                        :uint32)
          +foreign-array-length-mask+))

(declaim (inline foreign-array-type-of))
(defun foreign-array-type-of (pointer)
  "Return the type of a foreign array created by one of the
make-*-array utilities associated with a variable in WITH binding."
  (svref *foreign-array-types*
         (ash (cffi:mem-ref (cffi:inc-pointer pointer (- +foreign-header-size+))
                            :uint32)
              (- +foreign-array-length-bits+))))

(macrolet ((make-*-array (name type)
             `(defmacro ,name (&whole whole size &key zero-p initial-element
                               initial-contents)
                (declare (ignore zero-p initial-element))
                (let ((key-args (copy-list (cddr whole)))
                      (vals (gensym)))
                  (when initial-contents
                    (remf key-args :initial-contents))
                  (multiple-value-bind (binding init-key-value)
                      ;; The type of INITIAL-CONTENTS is only LIST but it is
                      ;; possible to initialize the array from other sequences
                      ;; within the INITIALIZE block.
                      (and initial-contents
                           (values `((,vals ,initial-contents))
                                   `(:initial-contents
                                     ;; Dummy value for the header.
                                     (and ,vals (cons (car ,vals) ,vals)))))
                    `(with ,binding
                       (%make-foreign-array ,size ,,type ,@key-args
                                            ,@init-key-value)))))))
  ;; A FRAME is a foreign array of SAMPLE type, useful to efficiently
  ;; store and return multiple values from a VUG
  (make-*-array make-frame 'sample)
  ;; Other utilities to create foreign arrays.
  (make-*-array make-i32-array :int32)
  (make-*-array make-u32-array :uint32)
  (make-*-array make-i64-array :int64)
  (make-*-array make-u64-array :uint64)
  (make-*-array make-f32-array :float)
  (make-*-array make-f64-array :double))

(setf
  (documentation 'make-frame 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type SAMPLE and size SIZE."
  (documentation 'make-i32-array 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type SIGNED-BYTE 32 and size SIZE."
  (documentation 'make-u32-array 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type UNSIGNED-BYTE 32 and size SIZE."
  (documentation 'make-i64-array 'function)
  "Associated with a variable in WITH binding to create and foreign
array of type SIGNED-BYTE 64 and size SIZE."
  (documentation 'make-u64-array 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type UNSIGNED-BYTE 64 and size SIZE."
  (documentation 'make-f32-array 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type SINGLE-FLOAT and size SIZE."
  (documentation 'make-f64-array 'function)
  "Associated with a variable in WITH binding to create a foreign
array of type DOUBLE-FLOAT and size SIZE.")

(defmacro make-pointer-array (size)
  "Associated with a variable in WITH binding to create a foreign
array of type foreign-pointer and size SIZE."
  `(%make-foreign-array ,size :pointer))

(declaim (inline %make-vec))
(defun %make-vec (size &key zero-p initial-element initial-contents)
  (cond ((or zero-p initial-element)
         (make-array size :initial-element (or initial-element 0)))
        (initial-contents
         (make-array size :initial-contents initial-contents))
        (t
         (make-array size))))

(defmacro maybe-make-x32-array-fname (name)
  `(reduce-warnings
     (if (< incudine.util::n-fixnum-bits 32) ',name '%make-vec)))

(defmacro maybe-make-i32-array (&whole whole size &key zero-p initial-element
                                initial-contents)
  "Associated with a variable in WITH binding to create an array of
type (signed-byte 32) and size SIZE. The array is foreign on 32-bit
platforms because the size of a fixnum is less than 32 bits."
  (declare (ignore zero-p initial-element initial-contents))
  (let ((fname (maybe-make-x32-array-fname make-i32-array)))
    `(,fname ,size ,@(cddr whole))))

(defmacro maybe-make-u32-array (&whole whole size &key zero-p initial-element
                                initial-contents)
  "Associated with a variable in WITH binding to create an array of
type (unsigned-byte 32) and size SIZE. The array is foreign on 32-bit
platforms because the size of a fixnum is less than 32 bits."
  (declare (ignore zero-p initial-element initial-contents))
  (let ((fname (maybe-make-x32-array-fname make-u32-array)))
    `(,fname ,size ,@(cddr whole))))

(defmacro maybe-i32-ref (array index)
  "Access the element of the array created by maybe-make-i32-array. Setfable."
  (if (< incudine.util::n-fixnum-bits 32)
      `(i32-ref ,array ,index)
      `(the fixnum (svref ,array ,index))))

(defmacro maybe-u32-ref (array index)
  "Access the element of the array created by maybe-make-u32-array. Setfable."
  (if (< incudine.util::n-fixnum-bits 32)
      `(u32-ref ,array ,index)
      `(the fixnum (svref ,array ,index))))

(declaim (inline frame-ref))
(defun frame-ref (ptr index)
  "Access the foreign array element of type SAMPLE specified by INDEX.
Setfable."
  (mem-ref ptr 'sample (the non-negative-fixnum
                         (* index +foreign-sample-size+))))

(define-compiler-macro frame-ref (ptr index)
  (if (constantp index)
      `(mem-ref ,ptr 'sample ,(* (eval index) +foreign-sample-size+))
      `(mem-ref ,ptr 'sample (the non-negative-fixnum
                               (* ,index ,+foreign-sample-size+)))))

(declaim (inline frame-set))
(defun frame-set (ptr index value)
  (setf (mem-ref ptr 'sample (the non-negative-fixnum
                               (* index +foreign-sample-size+)))
        value))

(define-compiler-macro frame-set (ptr index value)
  (if (constantp index)
      `(setf (mem-ref ,ptr 'sample ,(* (eval index) +foreign-sample-size+))
             ,value)
      `(setf (mem-ref ,ptr 'sample (the non-negative-fixnum
                                     (* ,index ,+foreign-sample-size+)))
             ,value)))

(defsetf frame-ref frame-set)

(defmacro multiple-sample-bind (vars frame &body body)
  "Used within the definition of a VUG, UGEN or DSP to bind the variables
VARS to the corresponding values in the foreign array of samples FRAME."
  (with-gensyms (frm)
    `(with ((,frm ,frame))
       (declare (type frame ,frm))
       (maybe-expand ,frm)
       (symbol-macrolet ,(loop for var in vars for count from 0
                               collect `(,var (frame-ref ,frm ,count)))
         ,@body))))

(defmacro samples (&rest values)
  "Used within the definition of a VUG, UGEN or DSP to return VALUES
as a foreign array of samples. The second returned value is the number
of samples."
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

(defmacro foreach-tick (&body forms)
  "Execute FORMS once at current time."
  (with-gensyms (old-time)
    `(with-samples ((,old-time -1.0d0))
       (unless (= (now) ,old-time)
         (setf ,old-time (now))
         ,@forms)
       (values))))

(defmacro foreach-channel (&body body)
  "Used within the definition of a VUG, UGEN or DSP to iterate over
the number of output channels with CURRENT-CHANNEL bound to each
number of channel.

Examples:

    (dsp! foreach-channel-example-1 (amp)
      (foreach-channel
        ;; Single noise generator for each output channel: the
        ;; generator doesn't depend on CURRENT-CHANNEL.
        (incf (audio-out current-channel) (white-noise amp))))

    ;; The same example with the VUG-MACRO COUT.
    (dsp! foreach-channel-example-1 (amp)
      (foreach-channel (cout (white-noise amp))))

    (dsp! foreach-channel-example-2 (amp)
      (vuglet ((randi (freq)
                 (interpolate (white-noise) freq))
               (sig (freq offset mult)
                 (:defaults 1 2000 1950)
                 (osc *sine-table* (+ offset (* mult (randi freq))) amp)))
        (foreach-channel
          ;; These generators depend on CURRENT-CHANNEL.
          ;; In this case there are four generators for the
          ;; first four output channels.
          (cout (case current-channel
                  (0 (sig 8))
                  (1 (sig 19))
                  (2 (sig 41))
                  (3 (sig 23))
                  (otherwise (sample 0)))))))"
  (with-gensyms (i)
    `(dochannels (,i *number-of-output-bus-channels*)
       (let ((current-channel ,i))
         (declare (type channel-number current-channel)
                  (ignorable current-channel))
         ,@body))))

(define-vug-macro counter (start end &key (step 1) loop-p done-action)
  "Count from START to END (excluded) by STEP, optionally in loop if
LOOP-P is T.

The one-argument function DONE-ACTION is called at the end if LOOP-P is NIL.
The function argument is the DSP node."
  (with-gensyms (counter)
    `(vuglet ((,counter ((start fixnum) (end fixnum) (step fixnum)
                         (loop-p boolean) (done-action function))
                (with ((done-p nil)
                       (index (progn (if done-p (setf done-p nil)) start)))
                  (declare (type fixnum index) (type boolean done-p))
                  (prog1 index
                    (unless done-p
                      (setf index
                            (cond ((< index end) (+ index step))
                                  (loop-p start)
                                  (t (funcall done-action (dsp-node))
                                     (setf done-p t)
                                     index))))))))
       (,counter ,start ,end ,step ,loop-p ,(or done-action '#'identity)))))

(define-vug downsamp ((control-period fixnum) input)
  "Downsampling of INPUT with CONTROL-PERIOD."
  (with ((count control-period)
         (value +sample-zero+))
    (declare (type fixnum count) (type sample value))
    (initialize (setf count 0))
    (if (<= count 1)
        (setf count control-period value input)
        (progn (decf count) value))))

(define-vug snapshot ((gate fixnum) (start-offset fixnum) input)
  "INPUT is updated every GATE samples, on demand or never.

If GATE is positive, the output is INPUT calculated every GATE samples.
If GATE is zero, the output is the old value of INPUT.
If GATE is negative, the output is the current value of INPUT and GATE
becomes zero.

START-OFFSET is the initial offset for the internal counter."
  (with-samples ((next-time (init-only (+ (now) gate)))
                 (value +sample-zero+))
    (initialize (setf next-time (+ (now) start-offset)))
    (cond ((plusp gate)
           (unless (< (now) next-time)
             (setf value (update input))
             (setf next-time (+ (now) gate))))
          ((minusp gate)
           (setf value (update input) gate 0)))
    value))

(define-vug %with-control-period ((gate fixnum) (start-offset fixnum) (input t))
  (with-samples ((next-time (init-only (+ (now) gate))))
    (initialize (setf next-time (+ (now) start-offset)))
    (cond ((plusp gate)
           (unless (< (now) next-time)
             (update input)
             (setf next-time (+ (now) gate))))
          ((minusp gate)
           (update input)
           (setf gate 0)))
    nil))

(define-vug-macro with-control-period ((n &optional (start-offset 0))
                                       &body body)
  "The BODY forms are updated every N samples, on demand or never.

If N is positive, BODY is updated every N samples.
If N is zero, BODY is not updated.
If N is negative, BODY is updated and N becomes zero.

START-OFFSET (0 by default) is the initial offset for the internal counter."
  (with-gensyms (kperiod)
    `(vuglet ((,kperiod ((gate fixnum) (start fixnum))
                (%with-control-period gate start (progn ,@body))))
       (,kperiod ,n ,start-offset))))

(define-vug samphold (input gate initial-value initial-threshold)
  "Sample and hold the INPUT whenever GATE decreases from one sample
to the next, for example when a phasor wraps around. This mechanism is
similar to the Pd-version.

INITIAL-VALUE and INITIAL-THRESHOLD default to 0 and 1 respectively."
  (:defaults (incudine:incudine-missing-arg "INPUT") 0 0 1)
  (with-samples ((threshold initial-threshold)
                 (value initial-value))
    (when (< gate threshold) (setf value input))
    (setf threshold gate)
    value))

(define-vug lin->lin (input old-min old-max new-min new-max)
  "Return a value between NEW-MIN and NEW-MAX that is the linear
mapping of the INPUT sample in the range of OLD-MIN to OLD-MAX."
  (with-samples ((old-rdelta (/ (sample 1) (- old-max old-min)))
                 (new-delta (- new-max new-min)))
    (+ new-min (* new-delta old-rdelta (- input old-min)))))

(define-vug lin->exp (in old-min old-max new-min new-max)
  "Return a value between NEW-MIN and NEW-MAX that is the exponential
mapping of the INPUT sample in the range of OLD-MIN to OLD-MAX."
  (with-samples ((old-rdelta (/ (sample 1) (- old-max old-min)))
                 (new-ratio (/ new-max new-min)))
    (* (expt (the non-negative-sample new-ratio)
             (* old-rdelta (- in old-min)))
       new-min)))

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
  "Destructively clip the INPUT sample to a value between LOW and HIGH."
  (%with-samples-rebinding ((lo low) (hi high))
    `(progn
       (cond ((> ,in ,hi) (setf ,in ,hi))
             ((< ,in ,lo) (setf ,in ,lo)))
       ,in)))

(defmacro wrap-cond ((in low high range &optional (offset in)) &rest clauses)
  (with-gensyms (r-range)
    `(with-samples ((,r-range (/ (sample ,range))))
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
                                                   (* ,offset ,r-range)))))))
                       clauses)))))

(define-vug-macro nwrap (in low high &optional range offset)
  "Destructively wrap-around the INPUT sample that exceeds the LOW and
HIGH thresholds."
  (%with-samples-rebinding ((lo low) (hi high))
    (let ((delta (or range hi)))
      `(progn
         (wrap-cond (,in ,lo ,hi ,delta ,(or offset in))
           ((>= ,in ,hi) (decf ,in ,delta))
           (,(if range `(< ,in ,lo) `(minusp ,in))
            (incf ,in ,delta)))
         ,in))))

(defmacro %mirror-consequent (in threshold1 threshold2 range two-range offset
                              offset-p bias)
  (with-gensyms (os r-two-range)
    `(with-samples ((,r-two-range (/ (sample ,two-range))))
       (setf ,in (- (+ ,threshold1 ,threshold1) ,in))
       (if (< ,in ,threshold2)
           (let ((,os ,offset))
             (setf ,in (- ,os (* ,two-range
                                 (sample->fixnum (* ,os ,r-two-range)))))
             (when (>= ,in ,range)
               (setf ,in (- ,two-range ,in)))
             ,(if offset-p `(+ ,in ,bias) in))
           ,in))))

(define-vug-macro nmirror (in low high &optional range two-range offset)
  "Destructively reflect the INPUT sample that exceeds the LOW and
HIGH thresholds."
  (%with-samples-rebinding ((lo low) (hi high))
    (let ((%range (or range hi))
          (%offset (or offset in)))
        `(progn
           (maybe-expand ,in)
           (cond ((zerop ,%range)
                  (setf ,in ,(if (eql hi %range) +sample-zero+ lo)))
                 ((>= ,in ,hi)
                  (%mirror-consequent ,in ,hi ,lo ,%range ,two-range ,%offset
                                      ,offset ,lo))
                 ((< ,in ,lo)
                  (%mirror-consequent ,in ,lo ,hi ,%range ,two-range ,%offset
                                      ,offset ,lo))
                 (t ,in))))))

(declaim (inline clip))
(defun clip (input low high)
  "Clip the INPUT sample to a value between LOW and HIGH."
  (flet ((%clip (in low high)
           (cond ((> in high) high)
                 ((< in low) low)
                 (t in))))
    (if (typep input 'sample)
        (%clip input (sample low) (sample high))
        (%clip input low high))))

(define-vug wrap (in low high)
  "Wrap-around the INPUT sample that exceeds the LOW and HIGH thresholds."
  (with-samples ((range (- high low))
                 (%in in))
    (nwrap %in low high range (- %in low))))

(define-vug mirror (in low high)
  "Reflect the INPUT sample that exceeds the LOW and HIGH thresholds."
  (with-samples ((range (- high low))
                 (two-range (+ range range))
                 (%in in))
    (nmirror %in low high range two-range (- in low))))

(define-vug-macro interpolate (generator-form freq
                               &optional (interpolation :linear) initial-value-p)
  "Interpolation of the values generated by a performance-time GENERATOR-FORM.

The values of the generator are calculated with a modulable frequency FREQ.

INTERPOLATION is one of :LINEAR (default), :COS, :CUBIC or NIL (sample-and-hold).

If INTERPOLATION is :CUBIC and INITIAL-VALUE-P is T, three adjacent
points are initialized with the same value.

Example:

    (define-vug randi (amp freq)
      (* amp (interpolate (white-noise) freq)))"
  (destructuring-bind (bindings init update result)
      (case interpolation
        ((:lin :linear)
         `(((x1 0) (delta 0))
           (setf x1 input)
           (setf x0 x1 x1 (update input) delta (- x0 x1))
           (+ x1 (* phase delta))))
        (:cos
         `(((x1 0))
           (setf x1 input)
           (setf x0 x1 x1 (update input))
           (cos-interp phase x1 x0)))
        (:cubic
         `(((x1 0) (x2 0) (x3 0))
           (setf x1 input
                 ;; Three adjacent points initialized with the same
                 ;; value when it is required an initial value.
                 x2 ,(if initial-value-p 'input `(update input))
                 x3 ,(if initial-value-p 'input `(update input)))
           (setf x0 x1 x1 x2 x2 x3 x3 (update input))
           (cubic-interp phase x3 x2 x1 x0)))
        (otherwise
         `(nil nil (setf x0 (update input)) x0)))
    (with-gensyms (interp)
      `(vuglet ((,interp (input freq)
                  (with-samples ((phase 0)
                                 (inc (* freq *sample-duration*))
                                 (x0 0)
                                 ,@bindings)
                    ,@(when init `((initialize ,init)))
                    (decf phase inc)
                    (when (minusp phase)
                      (setf phase (wrap phase 0 1))
                      ,update)
                    ,result)))
         (,interp ,generator-form ,freq)))))
