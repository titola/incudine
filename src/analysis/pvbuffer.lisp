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

(in-package :incudine.analysis)

(defstruct (pvbuffer (:constructor %make-pvbuffer)
                     (:copier nil))
  "PVbuffer type.

A PVBUFFER contains a sequence of spectral data."
  (data (null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (frames 0 :type non-negative-fixnum)
  (channels 1 :type channel-number)
  (fft-size 0 :type non-negative-fixnum)
  (scale-factor (sample 1) :type sample)
  (block-size 0 :type non-negative-fixnum))

(setf
  (documentation 'pvbuffer-data 'function)
  "Return the foreign pointer to the pvbuffer data."
  (documentation 'pvbuffer-size 'function)
  "Return the pvbuffer size."
  (documentation 'pvbuffer-channels 'function)
  "Return the number of the channels of the pvbuffer."
  (documentation 'pvbuffer-frames 'function)
  "Return the number of the spectral frames of the pvbuffer."
  (documentation 'pvbuffer-fft-size 'function)
  "Return the FFT size used to compute the spectral data of the pvbuffer."
  (documentation 'pvbuffer-scale-factor 'function)
  "Return the scale factor of the pvbuffer."
  (documentation 'pvbuffer-block-size 'function)
  "Return the block size of the pvbuffer.")

(defmethod incudine:free ((obj pvbuffer))
  (let ((data (pvbuffer-data obj)))
    (unless (null-pointer-p data)
      (free-multi-channel-data data (pvbuffer-channels obj))
      (incudine-cancel-finalization obj)
      (setf (pvbuffer-data obj) (null-pointer))
      (incudine.util:nrt-msg debug "Free ~A" (type-of obj)))
    (values)))

(defmethod incudine:free-p ((obj pvbuffer))
  (null-pointer-p (pvbuffer-data obj)))

(defmethod print-object ((obj pvbuffer) stream)
  (multiple-value-bind (size frames channels block-size)
      (if (incudine:free-p obj)
          (values 0 0 1 0)
          (values (pvbuffer-size obj) (pvbuffer-frames obj)
                  (pvbuffer-channels obj) (pvbuffer-block-size obj)))
    (format stream "#<PVBUFFER :SIZE ~D :FRAMES ~D :CHANNELS ~D :BLOCK-SIZE ~D>"
            size frames channels block-size)))

(declaim (inline number-of-partitions))
(defun number-of-partitions (frames partsize)
  (declare (type non-negative-fixnum frames partsize) #.*reduce-warnings*)
  (ceiling (/ frames partsize)))

(defmacro foreach-pvbuffer-channel ((pvbuf-var pvbuf-ptr-var pvbuf-pos-var
                                     buf-pos-var buf-offset channels)
                                    &body body)
  (with-gensyms (ch)
    `(dotimes (,ch ,channels)
       (let ((,pvbuf-ptr-var (cffi:mem-aref (pvbuffer-data ,pvbuf-var)
                                            :pointer ,ch))
             (,pvbuf-pos-var 0)
             (,buf-pos-var (+ (the non-negative-fixnum
                                (* ,buf-offset ,channels)) ,ch)))
         (declare (type non-negative-fixnum ,pvbuf-pos-var ,buf-pos-var))
         ,@body))))

(defmacro foreach-pvbuffer-frame (pvbuf-var &body body)
  `(loop repeat (pvbuffer-frames ,pvbuf-var) do ,@body))

(defmacro fft-input-from-buffer-partition (fft-inbuf fft-size partsize
                                           bufdata bufsize bufpos channels)
  (with-gensyms (index)
    `(progn
       ;; Fill the first half of the buffer (the FFT size is 2*partsize)
       (dotimes (,index ,partsize)
         (setf (smp-ref ,fft-inbuf ,index)
               (if (< ,bufpos ,bufsize)
                   (smp-ref ,bufdata ,bufpos)
                   +sample-zero+))
         (incf ,bufpos ,channels))
       ;; Zero-padding the second half of the buffer
       (loop for ,index from ,partsize below ,fft-size do
            (setf (smp-ref ,fft-inbuf ,index) +sample-zero+)))))

(defmacro fft-output-to-pvbuffer-frame (fft-outbuf pvbuf-data pvbuf-pos
                                        block-size)
  (with-gensyms (index)
    `(dotimes (,index ,block-size)
       (setf (smp-ref ,pvbuf-data ,pvbuf-pos)
             (smp-ref ,fft-outbuf ,index))
       (incf ,pvbuf-pos))))

(defun buffer->pvbuffer (buf partsize &key (start 0) (frames 0))
  "Create and return a new PVBUFFER structure with spectral data
obtained from a sequence of FFT's of the BUFFER structure BUF
partitioned with partition size PARTSIZE.

The optional keywords START and FRAMES mark the beginning position in
frames and the number of frames of the buffer, respectively."
  (declare (type incudine:buffer buf) (type positive-fixnum partsize)
           (type non-negative-fixnum start frames))
  (let ((fft (make-fft (* 2 partsize) :window-function #'rectangular-window))
        (channels (incudine:buffer-channels buf))
        (data nil)
        (obj nil))
    (handler-case
        (unwind-protect
          (let* ((bdata (incudine:buffer-data buf))
                 (bsize (incudine:buffer-size buf))
                 (partitions (number-of-partitions
                               (if (plusp frames)
                                   frames
                                   (incudine:buffer-frames buf))
                               partsize))
                 (block-size (fft-output-buffer-size fft))
                 (size (* partitions block-size))
                 (fft-size (fft-size fft))
                 (fft-inbuf (fft-input-buffer fft))
                 (fft-outbuf (fft-output-buffer fft)))
            (declare (type non-negative-fixnum partitions block-size size
                           fft-size)
                     #.*standard-optimize-settings*)
            (setf data (alloc-multi-channel-data channels size))
            (setf obj
                  (incudine-finalize
                    (reduce-warnings
                      (%make-pvbuffer :data data :size size :frames partitions
                                      :channels channels :fft-size fft-size
                                      :scale-factor (fft-scale-factor fft)
                                      :block-size block-size))
                    (lambda ()
                      (free-multi-channel-data data channels))))
            (foreach-pvbuffer-channel (obj pvbuf-ptr pvbpos bpos start channels)
              (foreach-pvbuffer-frame obj
                (fft-input-from-buffer-partition
                  fft-inbuf fft-size partsize bdata bsize bpos channels)
                (fft-execute (fft-plan fft) fft-inbuf fft-outbuf)
                (fft-output-to-pvbuffer-frame
                  fft-outbuf pvbuf-ptr pvbpos block-size)))
            obj)
          (free fft))
      (condition (c)
        (if data (free-multi-channel-data data channels))
        (error c)))))
