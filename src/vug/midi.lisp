;;; Copyright (c) 2013-2025 Tito Latini
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
  (defstruct (midi-table (:copier nil))
    (note-priority-vec (make-note-priority-vector))
    (note-velocity-vec (make-note-velocity-vector))
    (poly-aftertouch (make-array 128 :element-type '(integer 0)
                                 :initial-element 0))
    (cc (make-array 128 :element-type '(integer 0) :initial-element 0))
    (program 0 :type (integer 0))
    (global-aftertouch 0 :type (integer 0))
    ;; pitch bend between -8192 and 8191
    (pitch-bend 0 :type fixnum))

  ;;; Table to store the received MIDI channel messages.
  (defvar *midi-table*
    (make-array 16 :element-type 'midi-table
                :initial-contents (loop for i below 16
                                        collect (make-midi-table))))
  (declaim (type vector *midi-table*))

  (declaim (inline midi-channel))
  (defun midi-channel (status)
    (declare (type (unsigned-byte 8) status))
    (ldb (byte 4 0) status))

  (defun set-midi-message (status data1 data2)
    (declare (type (unsigned-byte 8) status data1 data2))
    (when (< #x7f status #xf0)
      (let ((chan (midi-channel status)))
        (let ((tab (svref *midi-table* chan)))
          (macrolet ((update-midi-msg (place value)
                       (with-gensyms (old)
                         `(let ((,old ,place))
                            (compare-and-swap ,place ,old ,value)))))
            (cond ((or (< status #x90)
                       (and (< status #xa0) (zerop data2)))
                   ;; Note off.
                   (rt-eval ()
                     (note-priority-remove (midi-table-note-priority-vec tab)
                                           (midi-table-note-velocity-vec tab)
                                           data1)))
                  ((< status #xa0)
                   ;; Note on.
                   (rt-eval ()
                     (note-priority-add (midi-table-note-priority-vec tab)
                                        (midi-table-note-velocity-vec tab)
                                        data1 data2)))
                  ((< status #xb0)
                   (update-midi-msg
                    (svref (midi-table-poly-aftertouch tab) data1) data2))
                  ((< status #xc0)
                   (update-midi-msg (svref (midi-table-cc tab) data1) data2))
                  ((< status #xd0)
                   (update-midi-msg (midi-table-program tab) data1))
                  ((< status #xe0)
                   (update-midi-msg (midi-table-global-aftertouch tab) data1))
                  (t (update-midi-msg (midi-table-pitch-bend tab)
                                      (+ (ash (- data2 64) 7) data1))))
            (values))))))

  (defun make-midi-normalize-table ()
    (let ((ptr (foreign-alloc-sample 128)))
      (declare #.*standard-optimize-settings*)
      (dotimes (i 128 ptr)
        (setf (smp-ref ptr i) (* i #.(/ (sample #x7f)))))))

  (defun make-midi-normalize-pb-bipolar-table ()
    (let ((ptr (foreign-alloc-sample 16384)))
      (declare #.*standard-optimize-settings*)
      (do ((i 0 (1+ i))
           (j -8192 (1+ j)))
          ((= i 16384) ptr)
        (declare (type fixnum i j))
        (setf (smp-ref ptr i) (* j (if (plusp j)
                                       #.(/ (sample 8191))
                                       #.(/ (sample 8192))))))))

  (defun make-midi-normalize-pb-table ()
    (let ((ptr (foreign-alloc-sample 16384)))
      (declare #.*standard-optimize-settings*)
      (dotimes (i 16384 ptr)
        (setf (smp-ref ptr i) (* i #.(/ (sample 16383)))))))

  ;;; Table used to normalize a data byte from [0, 0x7F] to [0.0, 1.0]
  (defvar *midi-normalize-table* (make-midi-normalize-table))

  ;;; Table used to normalize the value of the pitch bend
  ;;; from [-8192, 8191] to [-1.0, 1.0]
  (defvar *midi-normalize-pb-bipolar-table*
    (make-midi-normalize-pb-bipolar-table))

  ;;; Table used to normalize the value of the pitch bend to [0.0, 1.0]
  (defvar *midi-normalize-pb-table* (make-midi-normalize-pb-table))

  (defvar *linear-midi-table*
    (incudine:make-buffer 128
      :initial-contents (loop for i below 128 collect (/ i 127)))
    "BUFFER structure of size 128 for linear mapping from [0, 127]
to [0.0, 1.0].")
  (declaim (inline *linear-midi-table*))

  (declaim (inline midi-note-off-p midi-note-on-p midi-note-p
                   midi-poly-aftertouch-p midi-cc-p midi-program-p
                   midi-global-aftertouch-p midi-pitch-bend-p))

  (macrolet ((define-midi-*-p (spec)
               `(progn
                  ,@(mapcar
                      (lambda (cons)
                        `(defun ,(vug-format-symbol "MIDI-~A-P" (car cons))
                             (status)
                             ,(format nil "Return T if STATUS is the status byte ~
                                          of a MIDI ~(~A~). Otherwise, return NIL."
                                      (car cons))
                             (declare (type (unsigned-byte 8) status))
                             (= (ldb (byte 4 4) status) ,(cdr cons))))
                      spec)))
             (define-vug-midi (names)
               `(progn
                  ,@(mapcar (lambda (name)
                              `(define-vug ,(vug-format-symbol "MIDI-~A" name)
                                   ((channel fixnum))
                                 ,(format nil "Return the MIDI ~(~A~) for CHANNEL."
                                          name)
                                 (with ((table (svref *midi-table* channel)))
                                   (declare (type midi-table table))
                                   (the fixnum
                                     (,(vug-format-symbol "MIDI-TABLE-~A" name)
                                       table)))))
                            names))))
    (define-midi-*-p
        ((note-off . #x8) (note-on . #x9) (poly-aftertouch . #xa) (cc . #xb)
         (program . #xc) (global-aftertouch . #xd) (pitch-bend . #xe)))
    (define-vug-midi (program global-aftertouch)))

  (define-vug midi-velocity ((channel fixnum) (keynum (unsigned-byte 8)))
    "Return the velocity of the last received MIDI note for CHANNEL
and key number KEYNUM."
    (with ((velocity-vec (midi-table-note-velocity-vec
                           (svref *midi-table* channel))))
      (the (integer 0 127) (svref velocity-vec keynum))))

  (define-vug midi-poly-aftertouch ((channel fixnum) (keynum fixnum))
    "Return the last received MIDI poly-aftertouch for CHANNEL and key
number KEYNUM."
    (with ((pat-table (midi-table-poly-aftertouch (svref *midi-table* channel))))
      (declare (type simple-vector pat-table))
      (the fixnum (svref pat-table keynum))))

  (define-vug midi-cc ((channel fixnum) (number fixnum))
    "Return the value of the last received MIDI cc NUMBER for CHANNEL."
    (with ((cc-table (midi-table-cc (svref *midi-table* channel))))
      (declare (type simple-vector cc-table))
      (the fixnum (svref cc-table number))))

  (define-vug midi-pitch-bend ((channel fixnum))
    "Return the last received MIDI pitch bend for CHANNEL."
    (with ((table (svref *midi-table* channel)))
      (declare (type midi-table table))
      (midi-table-pitch-bend table)))

  (defmacro midi-linear-map (in min max &optional norm-table)
    (with-gensyms (delta)
      `(with-samples ((,delta (- ,max ,min)))
         (+ ,min (* ,delta (smp-ref ,(or norm-table '*midi-normalize-table*)
                                    ,in))))))

  (defmacro midi-exponential-map (in min max &optional norm-table)
    (with-gensyms (ratio)
      `(with-samples ((,ratio (/ ,max ,min)))
         (* ,min (expt (the non-negative-sample ,ratio)
                       (smp-ref ,(or norm-table '*midi-normalize-table*)
                                ,in)))))))

(defun midi-note-p (status)
  "Return T if STATUS is the status byte of a MIDI note. Otherwise,
return NIL."
  (declare (type (unsigned-byte 8) status))
  (or (midi-note-on-p status) (midi-note-off-p status)))

(define-vug midi-note-on ((channel fixnum))
  "Return the key number of the last received MIDI note-on message for
the CHANNEL."
  (with ((note-prio-vec (midi-table-note-priority-vec
                          (svref *midi-table* channel))))
    (the (integer 0 127) (%last-note-on note-prio-vec))))

(define-vug midi-note-off ((channel fixnum))
  "Return the key number of the last received MIDI note-off message
for the CHANNEL."
  (with ((note-prio-vec (midi-table-note-priority-vec
                          (svref *midi-table* channel))))
    (the (integer 0 127) (%last-note-off note-prio-vec))))

(define-vug midi-lowest-keynum ((channel fixnum))
  "Return the lowest received MIDI key number for CHANNEL."
  (with ((note-prio-vec (midi-table-note-priority-vec
                          (svref *midi-table* channel))))
    (the (integer 0 127) (lowest-note-priority note-prio-vec))))

(define-vug midi-highest-keynum ((channel fixnum))
  "Return the highest received MIDI key number for CHANNEL."
  (with ((note-prio-vec (midi-table-note-priority-vec
                          (svref *midi-table* channel))))
    (the (integer 0 127) (highest-note-priority note-prio-vec))))

(define-vug midi-cps ((tun tuning) (keynum (unsigned-byte 8)))
  "Return the frequency of the key number KEYNUM in a TUNING."
  (with ((data (tuning-data tun)))
    (declare (type pointer data))
    (smp-ref data keynum)))

(define-vug midi-amp ((ampbuf buffer) (channel fixnum)
                      (keynum (unsigned-byte 8)))
  "Return the amplitude that is the linear mapping of the last
received MIDI note for CHANNEL and key number KEYNUM.

AMPBUF is the BUFFER structure of size 128 used for linear mapping
from key number to amplitude."
  (with ((data (buffer-data ampbuf)))
    (smp-ref data (midi-velocity channel keynum))))

(define-vug lin-midi-poly-aftertouch ((channel fixnum) (keynum fixnum) min max)
  "Return a value between MIN and MAX that is the linear mapping of
the last received MIDI poly-aftertouch for CHANNEL and key number KEYNUM."
  (midi-linear-map (midi-poly-aftertouch channel keynum) min max))

(define-vug exp-midi-poly-aftertouch ((channel fixnum) (keynum fixnum) min max)
  "Return a value between MIN and MAX that is the exponential mapping of
the last received MIDI poly-aftertouch for CHANNEL and key number KEYNUM.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (midi-exponential-map (midi-poly-aftertouch channel keynum) min max))

(define-vug lin-midi-cc ((channel fixnum) (number fixnum) min max)
  "Return a value between MIN and MAX that is the linear mapping of
the last received MIDI cc NUMBER for CHANNEL."
  (midi-linear-map (midi-cc channel number) min max))

(define-vug exp-midi-cc ((channel fixnum) (number fixnum) min max)
  "Return a value between MIN and MAX that is the exponential mapping
of the last received MIDI cc NUMBER for CHANNEL.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (midi-exponential-map (midi-cc channel number) min max))

(define-vug lin-midi-global-aftertouch ((channel fixnum) min max)
  "Return a value between MIN and MAX that is the linear mapping of
the last received MIDI global-aftertouch for CHANNEL."
  (midi-linear-map (midi-global-aftertouch channel) min max))

(define-vug exp-midi-global-aftertouch ((channel fixnum) min max)
  "Return a value between MIN and MAX that is the exponential mapping
of the last received MIDI global-aftertouch for CHANNEL.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (midi-exponential-map (midi-global-aftertouch channel) min max))

(define-vug lin-midi-pitch-bend ((channel fixnum) min max)
  "Return a value between MIN and MAX that is the linear mapping of
the last received MIDI pitch bend for CHANNEL."
  (midi-linear-map (+ (midi-pitch-bend channel) 8192) min max
                   *midi-normalize-pb-bipolar-table*))

(define-vug exp-midi-pitch-bend ((channel fixnum) min max)
  "Return a value between MIN and MAX that is the exponential mapping
of the last received MIDI pitch bend for CHANNEL.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (midi-exponential-map (+ (midi-pitch-bend channel) 8192) min max
                        *midi-normalize-pb-table*))

(defun played-midi-note (note-number channel)
  "Return three values: the played MIDI NOTE-NUMBER starting from zero,
the related MIDI velocity and T if the note is played. This is useful
to create MIDI arpeggiators.

CHANNEL is the MIDI channel from 0 to 15.

Examples:

    ;; F7 chord [65 69 72 75] played on the first channel.
    (played-midi-note 1 0) ; => 69, 96, T
    (played-midi-note 3 0) ; => 75, 89, T
    (played-midi-note 6 0) ; =>  0,  0, NIL"
  (declare (type (unsigned-byte 8) note-number)
           (type (unsigned-byte 4) channel))
  (labels ((played (x i offset veloc-vec)
             (declare (type non-negative-fixnum x)
                      (type (unsigned-byte 8) i offset)
                      (type simple-vector veloc-vec))
             (let* ((y (integer-length (logand (lognot x) (1- x))))
                    (knum (+ y offset)))
               (declare (type (unsigned-byte 8) y knum))
               (if (zerop i)
                   (let ((vel (svref veloc-vec knum)))
                     (declare (type (unsigned-byte 8) vel))
                     (values knum vel (plusp vel)))
                   (played (logxor (ash 1 y) x) (1- i) offset veloc-vec))))
           (find-note (i n prio-vec veloc-vec)
             (declare (type (unsigned-byte 4) i)
                      (type (unsigned-byte 8) n)
                      (type simple-vector prio-vec veloc-vec))
             (if (>= i +note-priority-integers+)
                 (values 0 0 nil)
                 (let* ((x (svref prio-vec i))
                        (size (logcount x)))
                   (declare (type non-negative-fixnum x)
                            (type (unsigned-byte 8) size))
                   (if (> size n)
                       (played x n (svref +note-priority-offset-vector+ i)
                               veloc-vec)
                       (find-note (1+ i) (- n size) prio-vec veloc-vec))))))
    (declare #.*standard-optimize-settings*)
    (let ((mtab (svref *midi-table* channel)))
      (find-note 0 note-number (midi-table-note-priority-vec mtab)
                 (midi-table-note-velocity-vec mtab)))))

(defun reset-midi-notes (&optional channel)
  "Zero the MIDI note table for CHANNEL or all the tables if CHANNEL is NIL."
  (declare (type (or (unsigned-byte 4) null) channel))
  (if channel
      (let ((table (svref *midi-table* channel)))
        (reset-note-priority (midi-table-note-priority-vec table)
                             (midi-table-note-velocity-vec table)))
      (dotimes (i 16) (reset-midi-notes i))))

(defun midi-event-type-to-table-slot-name (type)
  (case type
    ((:note-on :note-off)                     'midi-table-note-priority-vec)
    (:velocity                                'midi-table-note-velocity-vec)
    ((:aftertouch :poly-aftertouch)           'midi-table-poly-aftertouch)
    (:control-change                          'midi-table-cc)
    (:program-change                          'midi-table-program)
    ((:channel-aftertouch :global-aftertouch) 'midi-table-global-aftertouch)
    (:pitch-bend                              'midi-table-pitch-bend)
    (otherwise (incudine-error "Unknown MIDI event type ~S" type))))

(defmacro incudine:with-midi-table-data (bindings &body body)
  "Create bindings to the values of the received MIDI messages.

Note: the received MIDI data are accessible if the :UPDATE-MIDI-TABLE-P
argument to INCUDINE:RECV-START is T (default).

BINDINGS is a list of lists

    (var midi-event-type channel)

where VAR is the variable bound to a MIDI table slot for the zero-based
CHANNEL, and MIDI-EVENT-TYPE selects the slot reader.

|---------------------+--------------------------------------------------------------|
| MIDI-EVENT-TYPE     | Slot description                                             |
|---------------------+--------------------------------------------------------------|
| :NOTE-ON            | Key number of the last received note-on message.             |
| :NOTE-OFF           | Key number of the last received note-off message.            |
| :VELOCITY           | Vector of velocities for each key number (0 after note-off). |
| :POLY-AFTERTOUCH    | Vector of poly-aftertouch values for each key number.        |
| :AFTERTOUCH         | Alias for :POLY-AFTERTOUCH                                   |
| :CONTROL-CHANGE     | Vector of control-change values for each control number.     |
| :PROGRAM-CHANGE     | Value of the last received MIDI program-change message.      |
| :CHANNEL-AFTERTOUCH | Value of the last received MIDI channel-aftertouch message.  |
| :GLOBAL-AFTERTOUCH  | Alias for :CHANNEL-AFTERTOUCH                                |
| :PITCH-BEND         | Value of the last received MIDI pitch-bend message.          |
|---------------------+--------------------------------------------------------------|

The slot value and the vector elements are SETFable (generally in rt-thread
if the value is read from DSP or UGEN instances in real-time).

Example:

    (with-midi-table-data ((last-note-on :note-on 0)
                           (cc-vec :control-change 0))
      (lambda ()
        (and (<= 48 last-note-on 60)
             (<= 64 (svref cc-vec 64) 127))))"
  (with-gensyms (tables)
    (let (let-bindings mac-bindings channel-var-alist)
      (loop for (var type chan) in bindings do
        (pushnew (cons chan (gensym "TABLE")) channel-var-alist :key 'car)
        (let* ((obj (list (midi-event-type-to-table-slot-name type)
                          (cdr (assoc chan channel-var-alist))))
               (vec (if (member type '(:note-on :note-off)) (gensym "NOTES"))))
          (if vec (push (list vec obj) let-bindings))
          (push (list var (case type
                            (:note-on `(%last-note-on ,vec))
                            (:note-off `(%last-note-off ,vec))
                            (otherwise obj)))
                mac-bindings)))
      `(let* ((,tables *midi-table*)
              ,@(loop for (chan . var) in (nreverse channel-var-alist)
                      collect `(,var (svref ,tables ,chan)))
              ,@(nreverse let-bindings))
         (symbol-macrolet ,mac-bindings ,@body)))))
