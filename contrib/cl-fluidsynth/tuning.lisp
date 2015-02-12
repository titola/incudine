;;; Copyright (c) 2015 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(set-fluidsynth-tuning set-tuning-from-fluidsynth)))

(defun %set-fluidsynth-tuning (synth bank prog tuning &optional (activate-p t))
  (declare (type fluidsynth:synth synth) (type (unsigned-byte 8) bank prog)
           (type tuning tuning) (type boolean activate-p))
  (cffi:with-foreign-object (pitch :double 128)
    (labels ((set-freqs (k i degrees os)
               (declare (type (unsigned-byte 8) k i degrees)
                        (type single-float os))
               (when (< k 128)
                 (setf (f64-ref pitch k)
                       (coerce (+ os (aref (tuning-cents tuning) i))
                               'double-float))
                 (let ((i (mod (1+ k) degrees)))
                   (set-freqs (1+ k) i degrees
                              (if (zerop i)
                                  (+ os (aref (tuning-cents tuning) degrees))
                                  os))))))
      (set-freqs 0 0 (1- (length (tuning-cents tuning)))
                 (incudine::tuning-et12-cents-offset tuning))
      (fluidsynth:activate-key-tuning synth bank prog
                                      (tuning-description tuning)
                                      pitch activate-p))))

(defun set-fluidsynth-tuning (obj synth bank prog &key description
                              (keynum-base 69) (freq-base 440) (degree-index 9)
                              (activate-p t))
  "Set the FluidSynth tuning with BANK and PROG preset. OBJ is a list of
notes, a scl file or a INCUDINE:TUNING structure. KEYNUM-BASE, FREQ-BASE
and DEGREE-INDEX are used as reference to generate the tuning frequencies
when OBJ is a list of notes or a file. If ACTIVATE-P is T (default), apply
the new tuning in realtime to existing notes which are using the replaced
tuning."
  (declare (type (or tuning list string pathname) obj)
           (type fluidsynth:synth synth)
           (type (unsigned-byte 8) bank prog keynum-base degree-index)
           (type (or string null) description) (type real freq-base)
           (type boolean activate-p))
  (if (tuning-p obj)
      (%set-fluidsynth-tuning synth bank prog obj activate-p)
      (let ((tun (apply #'make-tuning
                        (append (list (if (listp obj) :notes :file) obj)
                                (list :description description
                                      :keynum-base keynum-base
                                      :freq-base freq-base
                                      :degree-index degree-index)))))
        (unwind-protect
             (%set-fluidsynth-tuning synth bank prog tun activate-p)
          (free tun)))))

(define-constant +fluidsynth-max-name-size+ 80)

(defun set-tuning-from-fluidsynth (tuning synth bank prog)
  (declare (type tuning tuning) (type fluidsynth:synth synth)
           (type (unsigned-byte 8) bank prog))
  "The frequencies of a TUNING structure are obtained from the FluidSynth
tuning with BANK and PROG preset. The TUNING description is updated with the
name of the FluidSynth tuning."
  (cffi:with-foreign-object (name :char +fluidsynth-max-name-size+)
    (cffi:with-foreign-object (pitch :double 128)
      (fluidsynth:tuning-dump synth bank prog name +fluidsynth-max-name-size+
                              pitch)
      (let ((data (tuning-data tuning)))
        (dotimes (i 128)
          (setf (smp-ref data i)
                (sample (* 8.1758 (expt 2 (/ (f64-ref pitch i) 1200))))))
        (setf (tuning-description tuning) (cffi:foreign-string-to-lisp name))
        tuning))))
