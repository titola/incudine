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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (midi-table (:copier nil))
    (note 0 :type (integer 0))
    (poly-aftertouch (make-array 128 :element-type '(integer 0)
                                 :initial-element 0))
    (cc (make-array 128 :element-type '(integer 0) :initial-element 0))
    (program 0 :type (integer 0))
    (global-aftertouch 0 :type (integer 0))
    ;; pitch bend between -8192 and 8191
    (pitch-bend 0 :type fixnum))

  (defvar *midi-table*
    (make-array 16 :element-type 'midi-table
                :initial-contents (loop for i below 16
                                        collect (make-midi-table)))
    "Table to store the received MIDI channel messages")
  (declaim (type vector *midi-table*))

  (declaim (inline midi-channel))
  (defun midi-channel (status)
    (declare (type (unsigned-byte 8) status))
    (ldb (byte 4 0) status))

  (declaim (inline set-midi-message))
  (defun set-midi-message (status data1 data2)
    (declare (type (unsigned-byte 8) status data1 data2))
    (when (< #x7f status #xf0)
      (let ((chan (midi-channel status)))
        (let ((tab (svref *midi-table* chan)))
          (macrolet ((update-midi-msg (place value)
                       (with-gensyms (old)
                         `(let ((,old ,place))
                            (compare-and-swap ,place ,old ,value)))))
            (cond ((< status #xa0)
                   (update-midi-msg (midi-table-note tab)
                                    (+ (ash data1 8) data2)))
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

  (defun create-equal-temperament (steps freq-base keynum-base)
    (declare (type positive-fixnum steps)
             (type alexandria:positive-real freq-base keynum-base))
    (lambda (keynum)
      (declare (type (integer 0 127) keynum))
      (* freq-base (expt 2 (* (- keynum keynum-base) (/ 1.0 steps))))))

  ;;; Table used to get the cycles for seconds from the value of a MIDI keynum.
  (defvar *midi-frequency-table*
    (cffi:foreign-alloc 'sample :count 128
      :initial-contents (loop for i below 128
                              with fn = (create-equal-temperament 12 440 69)
                              collect (sample (funcall fn i)))))

  ;;; Table used to normalize a data byte from [0, 0x7F] to [0.0, 1.0]
  (defvar *midi-normalize-table*
    (cffi:foreign-alloc 'sample :count 128
      :initial-contents (loop for i below 128
                              with r-midi-data2-max = (/ (sample #x7f))
                              collect (* i r-midi-data2-max))))

  ;;; Table used to normalize the value of the pitch bend
  ;;; from [-8192, 8191] to [-1.0, 1.0]
  (defvar *midi-normalize-pb-bipolar-table*
    (cffi:foreign-alloc 'sample :count 16384
      :initial-contents (loop for i from -8192 to 8191
                              with r1 = (/ (sample 8192))
                              with r2 = (/ (sample 8191))
                              collect (* i (if (plusp i) r2 r1)))))

  ;;; Table used to normalize the value of the pitch bend to [0.0, 1.0]
  (defvar *midi-normalize-pb-table*
    (cffi:foreign-alloc 'sample :count 16384
      :initial-contents (loop for i from 0 below 16384
                              with r = (/ (sample 16383))
                              collect (* i r))))

  ;;; Table used to get the amplitude from the value of a MIDI velocity.
  ;;; The default is a linear mapping from [0, 0x7F] to [0.0, 1.0]
  (defvar *midi-amplitude-table*
    (let ((ptr (foreign-alloc-sample 128)))
      (foreign-copy ptr *midi-normalize-table* (* 128 +foreign-sample-size+))
      ptr)))

(declaim (inline midi-note-off-p midi-note-on-p midi-note-p
                 midi-poly-aftertouch-p midi-cc-p midi-program-p
                 midi-global-aftertouch-p midi-pitch-bend-p))

(macrolet ((define-midi-*-p (spec)
             `(progn
                ,@(mapcar (lambda (cons)
                            `(defun ,(vug-format-symbol "MIDI-~A-P" (car cons))
                                 (status)
                               (declare (type (unsigned-byte 8) status))
                               (= (ldb (byte 4 4) status) ,(cdr cons))))
                          spec)))
           (define-vug-midi (names)
             `(progn
                ,@(mapcar (lambda (name)
                            `(define-vug ,(vug-format-symbol "MIDI-~A" name)
                                 ((channel fixnum))
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

(defun midi-note-p (status)
  (declare (type (unsigned-byte 8) status))
  (or (midi-note-on-p status) (midi-note-off-p status)))

(define-vug midi-note ((channel fixnum))
  (with ((table (svref *midi-table* channel)))
    (declare (type midi-table table))
    (the (unsigned-byte 16) (midi-table-note table))))

(define-vug midi-keynum ((channel fixnum))
  (ldb (byte 8 8) (midi-note channel)))

(define-vug midi-velocity ((channel fixnum))
  (logand (midi-note channel) #x7f))

(define-vug midi-poly-aftertouch ((channel fixnum) (keynum fixnum))
  (with ((pat-table (midi-table-poly-aftertouch (svref *midi-table* channel))))
    (declare (type simple-vector pat-table))
    (the fixnum (svref pat-table keynum))))

(define-vug midi-cc ((channel fixnum) (number fixnum))
  (with ((cc-table (midi-table-cc (svref *midi-table* channel))))
    (declare (type simple-vector cc-table))
    (the fixnum (svref cc-table number))))

(define-vug midi-pitch-bend ((channel fixnum))
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
                              ,in))))))

(define-vug lin-midi-poly-aftertouch ((channel fixnum) (keynum fixnum) min max)
  (midi-linear-map (midi-poly-aftertouch channel keynum) min max))

(define-vug exp-midi-poly-aftertouch ((channel fixnum) (keynum fixnum) min max)
  (midi-exponential-map (midi-poly-aftertouch channel keynum) min max))

(define-vug lin-midi-cc ((channel fixnum) (number fixnum) min max)
  (midi-linear-map (midi-cc channel number) min max))

(define-vug exp-midi-cc ((channel fixnum) (number fixnum) min max)
  (midi-exponential-map (midi-cc channel number) min max))

(define-vug lin-midi-global-aftertouch ((channel fixnum) min max)
  (midi-linear-map (midi-global-aftertouch channel) min max))

(define-vug exp-midi-global-aftertouch ((channel fixnum) min max)
  (midi-exponential-map (midi-global-aftertouch channel) min max))

(define-vug lin-midi-pitch-bend ((channel fixnum) min max)
  (midi-linear-map (+ (midi-pitch-bend channel) 8192) min max
                   *midi-normalize-pb-bipolar-table*))

(define-vug exp-midi-pitch-bend ((channel fixnum) min max)
  (midi-exponential-map (+ (midi-pitch-bend channel) 8192) min max
                        *midi-normalize-pb-table*))
