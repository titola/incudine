;;; Copyright (c) 2013-2018 Tito Latini
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

(in-package :incudine.voicer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'keynum->cps)
    ;;; Default function to fill the table of the frequencies (12-tET).
    (setf (symbol-function 'keynum->cps)
          (lambda (k &optional (tuning incudine:*default-tuning*))
            (coerce (incudine:tuning-cps tuning k) 'single-float)))))

(defvar *default-midi-amplitude-array*
  ;; Linear map from velocity to amplitude.
  (coerce (loop for i below 128
                with r-midi-velocity-max = (/ 1.0 #x7f)
                collect (* i r-midi-velocity-max)) 'vector))
(declaim (type simple-vector *default-midi-amplitude-array*))

;;; Default function to fill the table of the amplitudes.
(declaim (inline velocity->amp))
(defun velocity->amp (velocity)
  (declare (type (integer 0 127) velocity))
  (svref *default-midi-amplitude-array* velocity))

(defstruct (midi-event (:include event) (:copier nil))
  "MIDI event type for voice management."
  (channel 0 :type (integer 0 15))
  (lokey 0 :type (integer 0 127))
  (hikey 127 :type (integer 0 127))
  ;; Table of the frequencies.
  (freq-table (make-array 128 :element-type 'single-float)
              :type (simple-array single-float (128)))
  ;; Table of the amplitudes.
  (amp-table  (make-array 128 :element-type 'single-float)
              :type (simple-array single-float (128)))
  ;; Ignore the note-off message if NOTE-OFF-P is NIL.
  (note-off-p t :type boolean))

(defmethod print-object ((obj midi-event) stream)
  (format stream "#<MIDI-EVENT :VOICER ~A~%~13t:RESPONDER ~A>"
          (event-voicer obj) (event-responder obj)))

(defun set-freq-table-from-portmidi (ev stream)
  (declare (type midi-event ev) (type pm:input-stream stream))
  (pm:with-input-sysex-event (ptr stream)
    (when (and (>= (pm:input-stream-events-remain stream)
                   (ash incudine::+midi-bulk-tuning-dump-buffer-size+ -2))
               (incudine::valid-midi-bulk-tuning-dump-p ptr nil 8))
      (cffi:with-pointer-to-vector-data (freqs (midi-event-freq-table ev))
        (let ((ret (cffi:foreign-funcall "set_ffreqs_from_midi" :pointer ptr
                                         :pointer freqs :int)))
          (declare (type fixnum ret))
          (if (zerop ret)
              (msg debug "received MIDI bulk tuning dump")
              (msg warn "MIDI bulk tuning dump failed at index ~D" ret)))))))

#+jack-midi
(defun set-freq-table-from-jackmidi (ev stream)
  (declare (type midi-event ev) (type jackmidi:input-stream stream))
  (multiple-value-bind (ptr size) (jackmidi:input-stream-sysex-pointer stream)
    (declare (type cffi:foreign-pointer ptr) (type non-negative-fixnum size))
    (when (and (= size incudine::+midi-bulk-tuning-dump-buffer-size+)
               (incudine::valid-midi-bulk-tuning-dump-p ptr nil 4))
      (cffi:with-pointer-to-vector-data (freqs (midi-event-freq-table ev))
        (let ((ret (cffi:foreign-funcall "set_ffreqs_from_midi_data_format"
                     :pointer freqs
                     :pointer (cffi:inc-pointer ptr
                                incudine::+midi-bulk-tuning-dump-freq-data-index+)
                     :unsigned-int 128
                     :int)))
          (declare (type fixnum ret))
          (if (zerop ret)
              (msg debug "received MIDI bulk tuning dump")
              (msg warn "MIDI bulk tuning dump failed at index ~D" ret)))))))

;;; Fill the frequency table with the data received from a MIDI bulk
;;; tuninig dump message.
(declaim (inline set-freq-table-from-midi))
(defun set-freq-table-from-midi (ev stream)
  (declare (type midi-event ev) (type incudine::midi-input-stream stream))
  #-jack-midi
  (set-freq-table-from-portmidi ev stream)
  #+jack-midi
  (if (pm:input-stream-p stream)
      (set-freq-table-from-portmidi ev stream)
      (set-freq-table-from-jackmidi ev stream)))

(defun %fill-table (ev obj table-getter)
  (declare (type (or function incudine:tuning incudine:buffer) obj))
  (let ((len (if (functionp obj)
                 128
                 (min (incudine::buffer-base-size obj) 128))))
    (declare (type positive-fixnum len))
    (dotimes (i len ev)
      (setf (aref (funcall table-getter ev) i)
            (coerce (if (functionp obj)
                        (funcall obj i)
                        (incudine:buffer-value obj i))
                    'single-float)))))

(defun fill-freq-table (midi-event obj)
  "Fill the table of the frequencies of MIDI-EVENT with the content of
a INCUDINE:TUNING, INCUDINE:BUFFER or by calling a function with the
key number as argument.

If OBJ is NIL, use the default function."
  (declare (type (or function incudine:tuning incudine:buffer null) obj)
           (type midi-event midi-event))
  (%fill-table midi-event (or obj (event-freq-function midi-event))
               #'midi-event-freq-table))

(defun fill-amp-table (midi-event obj)
  "Fill the table of the amplitudes of MIDI-EVENT with the content of
a INCUDINE:BUFFER or by calling a function with the key number as
argument.

If OBJ is NIL, use the default function."
  (declare (type (or function incudine:buffer null) obj)
           (type midi-event midi-event))
  (%fill-table midi-event
               (or obj
                   (lambda (vel)
                     (* (funcall (event-amp-function midi-event) vel)
                        (event-amp-mult midi-event))))
               #'midi-event-amp-table))

(defun scale-midi-amp (midi-event mult)
  "Scale the table of the amplitudes of MIDI-EVENT by MULT."
  (declare (type midi-event midi-event) (type real mult))
  (setf (event-amp-mult midi-event) mult)
  (fill-amp-table midi-event nil)
  mult)

(defmacro responder-noteon-form ((voicer note-off-p keynum velocity) &body body)
  `(progn
     ,(if note-off-p
          `(if (plusp ,velocity)
               (progn ,@body)
               (unsafe-release ,voicer ,keynum))
          `(progn ,@body))
     (values)))

(defmacro midi-bind (voicer stream &key (channel 0) (lokey 0) (hikey 127)
                     (freq-keyword :freq) freq-function (amp-keyword :amp)
                     (amp-mult 0.2) amp-function (gate-keyword :gate)
                     (gate-value 1) (note-off-p t))
  "Create and return a new MIDI-EVENT structure bound to VOICER and MIDI
input STREAM.

Allocate a new voice if the received MIDI channel is CHANNEL (0 by default)
and the MIDI key number is an integer between LOKEY and HIKEY (0 and 127 by
default).

The keywords FREQ-KEYWORD, AMP-KEYWORD and GATE-KEYWORD related to the
control parameters to set the DSP frequency, amplitude and gate, default to
:FREQ, :AMP and :GATE respectively.

GATE-VALUE is the value of the control parameter gate and defaults to 1.

If FREQ-FUNCTION is a function of one argument, it is used to convert
from MIDI key number to frequency value in Hz.

If AMP-FUNCTION is a function of one argument, it is used to convert
from MIDI velocity to amplitude value.

If NOTE-OFF-P is T (default), a note-on message (status byte 9) with
velocity 0 is interpreted as a note-off message."
  (with-gensyms (event status data1 data2 typ)
    `(let ((,event (make-midi-event :voicer ,voicer :channel ,channel
                     :lokey ,lokey :hikey ,hikey :freq-keyword ,freq-keyword
                     :freq-function ,(or freq-function `(function keynum->cps))
                     :amp-keyword ,amp-keyword :amp-mult ,amp-mult
                     :amp-function ,(or amp-function `(function velocity->amp))
                     :gate-keyword ,gate-keyword :gate-value ,gate-value
                     :note-off-p ,note-off-p)))
       (fill-freq-table (fill-amp-table ,event nil) nil)
       (setf (event-responder ,event)
             (incudine:make-responder ,stream
              (compile nil
                (lambda (,status ,data1 ,data2)
                  (declare (type (integer 0 255) ,status)
                           (type (integer 0 127) ,data1 ,data2))
                  (incudine-optimize
                    (when (and (= (ldb (byte 4 0) ,status) ,channel)
                               (<= ,lokey ,data1)
                               (<= ,data1 ,hikey))
                      (let ((,typ (ldb (byte 4 4) ,status)))
                        (cond
                          ((pm:sysex-message-p ,status)
                           (set-freq-table-from-midi ,event ,stream))
                          ((= ,typ 9)
                           (with-lock (,voicer)
                             (responder-noteon-form (,voicer ,note-off-p ,data1
                                                     ,data2)
                               (unsafe-set-controls ,voicer
                                 ,@(if freq-keyword
                                       `(,freq-keyword
                                         (aref (midi-event-freq-table ,event)
                                               ,data1))
                                       `(:keynum ,data1))
                                 ,@(if amp-keyword
                                       `(,amp-keyword
                                         (aref (midi-event-amp-table ,event)
                                               ,data2))
                                       `(:velocity ,data2))
                                 ,@(if gate-keyword
                                       `(,gate-keyword ,gate-value)))
                               (unsafe-trigger ,voicer ,data1))))
                          ,@(if note-off-p
                                `(((= ,typ 8) (release ,voicer ,data1)))))))
                    (values))))))
       ,event)))
