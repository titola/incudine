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

(in-package :incudine.voicer)

;;; TODO: it is provisory
(declaim (inline keynum->cps))
(defun keynum->cps (keynum)
  (declare (type (integer 0 127) keynum))
  (* 440.0 (expt 2.0 (* (- keynum 69) 0.083333333333))))

(defstruct (midi-event (:copier nil))
  voicer
  responder
  (channel 0 :type (integer 0 15))
  (lokey 0 :type (integer 0 127))
  (hikey 127 :type (integer 0 127))
  (freq-keyword :freq :type keyword)
  (freq-vector (make-array 128) :type (simple-vector 128))
  (tuning nil)  ; TODO
  (amp-keyword :amp :type keyword)
  (amp-mult 0.2)
  (amp-vector (make-array 128) :type (simple-vector 128))
  (gate-keyword :gate :type keyword)
  (gate-value 1.0)
  (note-off-p t :type boolean))

(defmethod print-object ((obj midi-event) stream)
  (format stream "#<MIDI-EVENT :VOICER ~A~%~13t:RESPONDER ~A>"
          (midi-event-voicer obj) (midi-event-responder obj)))

(declaim (inline fill-amp-vector))
(defun fill-amp-vector (function midi-event)
  (declare (type function function) (type midi-event midi-event))
  (dotimes (i 128 midi-event)
    (setf (svref (midi-event-amp-vector midi-event) i)
          (funcall function i))))

(declaim (inline fill-freq-vector))
(defun fill-freq-vector (function midi-event)
  (declare (type function function) (type midi-event midi-event))
  (dotimes (i 128 midi-event)
    (setf (svref (midi-event-freq-vector midi-event) i)
          (funcall function i))))

(declaim (inline update-freq-vector))
(defun update-freq-vector (midi-event)
  (declare (type midi-event midi-event))
  (fill-freq-vector #'keynum->cps midi-event))

(declaim (inline update-amp-vector))
(defun update-amp-vector (midi-event)
  (declare (type midi-event midi-event))
  (fill-amp-vector (lambda (vel)
                      (* vel (midi-event-amp-mult midi-event) 0.007874016))
                    midi-event))

;;; If NOTE-OFF-P is T, a note-on message (status byte 9) with velocity 0
;;; is interpreted as a note-off message.
(defmacro responder-noteon-form (voicer note-off-p keynum velocity &body body)
  (if note-off-p
      `(if (plusp ,velocity)
           (progn ,@body)
           (unsafe-release ,voicer ,keynum))
      `(progn ,@body)))

(defmacro midi-bind (voicer stream
                     &key (channel 0) (lokey 0) (hikey 127)
                     (freq-keyword :freq) (amp-keyword :amp)
                     (amp-mult 0.2) (gate-keyword :gate)
                     (gate-value 1) (note-off-p t))
  (with-gensyms (event status data1 data2 typ)
    `(let ((,event (make-midi-event :voicer ,voicer
                     :channel ,channel :lokey ,lokey :hikey ,hikey
                     :freq-keyword ,freq-keyword :amp-keyword ,amp-keyword
                     :amp-mult ,amp-mult :gate-keyword ,gate-keyword
                     :gate-value ,gate-value :note-off-p ,note-off-p)))
       (update-freq-vector (update-amp-vector ,event))
       (setf (midi-event-responder ,event)
             (incudine:make-responder ,stream
                (lambda (,status ,data1 ,data2)
                  (declare #.*standard-optimize-settings*
                           (type (integer 0 255) ,status)
                           (type (integer 0 127) ,data1 ,data2))
                  (when (and (= (ldb (byte 4 0) ,status) ,channel)
                             (<= ,lokey ,data1)
                             (<= ,data1 ,hikey))
                    (let ((,typ (ldb (byte 4 4) ,status)))
                      (cond ((= ,typ 9)
                             (with-safe-change (,voicer)
                               (responder-noteon-form ,voicer ,note-off-p ,data1 ,data2
                                 (unsafe-set-controls ,voicer
                                   ,@(if freq-keyword
                                         `(,freq-keyword
                                           (the single-float
                                             (svref (midi-event-freq-vector ,event)
                                                    ,data1))))
                                   ,@(if amp-keyword
                                         `(,amp-keyword
                                           (the single-float
                                             (svref (midi-event-amp-vector ,event)
                                                    ,data2))))
                                   ,@(if gate-keyword
                                         `(,gate-keyword ,gate-value)))
                                 (unsafe-trigger ,voicer ,data1))))
                            ,@(if note-off-p
                                  `(((= ,typ 8)
                                     (release ,voicer ,data1))))))))))
       ,event)))

(defgeneric (setf event-amp-mult) (value obj))

(defmethod (setf event-amp-mult) (value (obj midi-event))
  (setf (slot-value obj 'amp-mult) value)
  (update-amp-vector obj)
  value)
