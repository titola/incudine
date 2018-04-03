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

(in-package :incudine.vug)

(define-vug direct-convolve (in (buf buffer))
  "Direct convolution of an input with a finite impulse response
stored in a buffer."
  (with ((kernel (buffer-data buf))
         (size (buffer-size buf))
         (data (make-frame size :zero-p t))
         (pos 0)
         (sum +sample-zero+))
    (declare (type fixnum pos) (type sample sum) (type frame data))
    (labels ((conv (index kernel-pos end)
               (declare (type fixnum index kernel-pos end))
               (cond ((< index end)
                      (incf sum (* (smp-ref data index)
                                   (smp-ref kernel kernel-pos)))
                      (conv (1+ index) (1+ kernel-pos) end))
                     (t kernel-pos))))
      (setf (smp-ref data pos) in)
      (setf sum (* in (smp-ref kernel 0)))
      (conv 0 (conv (1+ pos) 1 size) pos)
      (setf pos (1- (if (zerop pos) size pos)))
      sum)))

;;;
;;; Partitioned convolution.
;;;
;;; References:
;;;
;;;   [1] T. G. Stockham Jr., "High-speed convolution and correlation",
;;;   AFIPS Proc. 1966 Spring Joint Computer Conf., Vol. 28, Spartan Books,
;;;   1966, pp. 229 - 233.
;;;
;;;   [2] J. S. Soo, K. K. Pang, "Multidelay block frequency adaptive filter",
;;;   IEEE Trans. Acoust. Speech Signal Process., Vol. ASSP-38, No. 2,
;;;   February 1990.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine.analysis:pvbuffer
     incudine.analysis:pvbuffer-data
     incudine.analysis:pvbuffer-size
     incudine.analysis:pvbuffer-frames
     incudine.analysis:pvbuffer-channels
     incudine.analysis:pvbuffer-fft-size
     incudine.analysis:pvbuffer-block-size
     incudine.analysis:pvbuffer-scale-factor
     incudine.external:pconv-multiply-partitions)))

(defmacro pconv-update-frame (frame output-index-var outbuf
                              channels mult)
  (with-gensyms (ch i)
    `(do ((,ch 0 (1+ ,ch))
          (,i ,output-index-var (1+ ,i)))
         ((>= ,ch ,channels) (setf ,output-index-var ,i))
       (declare (type non-negative-fixnum ,ch ,i))
       (setf (frame-ref ,frame ,ch)
             (* ,mult (smp-ref ,outbuf ,i))))))

(defmacro pconv-update-input (fft-inbuf index value pad-index-var)
  `(progn
     (setf (smp-ref ,fft-inbuf ,index) ,value)
     ;; Zero padding of the second half
     (setf (smp-ref ,fft-inbuf ,pad-index-var) +sample-zero+)
     (incf ,pad-index-var)
     (incf ,index)))

(declaim (inline pconv-update-output))
(defun pconv-update-output (partsize channel channels
                            outbuf outbuf-half ifft-outbuf)
  (labels ((rec (i j k l)
             (declare (type non-negative-fixnum i j k l))
             (when (< i partsize)
               (setf (smp-ref outbuf k) (+ (smp-ref ifft-outbuf i)
                                           ;; Overlap the the tail of the
                                           ;; prior computation
                                           (smp-ref outbuf l)))
               ;; Update the tail to overlap at the next block
               (setf (smp-ref outbuf l) (smp-ref ifft-outbuf j))
               (rec (1+ i) (1+ j) (+ k channels) (+ l channels)))))
    (rec 0 partsize channel (+ channel outbuf-half))))

(declaim (inline update-fdl))
(defun update-fdl (fdl fft-outbuf start block-size)
  (declare (type non-negative-fixnum start block-size))
  (do ((i 0 (1+ i))
       (j start (1+ j)))
      ((>= i block-size))
    (declare (type non-negative-fixnum i j))
    (setf (smp-ref fdl j) (smp-ref fft-outbuf i))))

(defmacro update-fdl-head (head-var block-size last)
  `(setf ,head-var (if (plusp ,head-var)
                       ;; Decrement the FDL
                       (- ,head-var ,block-size)
                       ;; Last partition of the FDL
                       ,last)))

(define-vug part-convolve (in (pvbuf pvbuffer))
  "Partitioned convolution. The PVBUFFER contains the partitioned FFT
of a multichannel finite impulse response."
  (with ((fft-size (pvbuffer-fft-size pvbuf))
         (half-size (ash fft-size -1))
         (partitions (pvbuffer-frames pvbuf))
         (channels (pvbuffer-channels pvbuf))
         (block-size (pvbuffer-block-size pvbuf))
         (scale-factor (pvbuffer-scale-factor pvbuf))
         ;; Impulse response
         (irdata (pvbuffer-data pvbuf))
         (fdl-size (pvbuffer-size pvbuf))
         ;; Frequency Delay Line
         (fdl (make-frame fdl-size :zero-p t))
         (fdl-last (- fdl-size block-size))
         (fdl-head 0)
         (fft (make-fft fft-size :window-function #'rectangular-window))
         (ifft (make-ifft fft-size :window-function #'rectangular-window))
         (fft-inbuf (fft-input-buffer fft))
         (fft-outbuf (fft-output-buffer fft))
         (ifft-inbuf (ifft-input-buffer ifft))
         (ifft-outbuf (ifft-output-buffer ifft))
         (outbuf-size (the non-negative-fixnum (* channels fft-size)))
         ;; Output buffer
         (outbuf (make-frame outbuf-size :zero-p t))
         (outbuf-half (ash outbuf-size -1))
         (input-index 0)
         (pad-index half-size)
         (output-index (if (plusp fft-size)
                           ;; Reinitialize the indexes if the PVBUFFER is changed
                           (setf fdl-head 0 input-index 0)
                           0))
         (frame (make-frame channels)))
    (declare (type non-negative-fixnum fft-size half-size partitions channels
                   block-size fdl-size fdl-last fdl-head outbuf-half
                   output-index input-index pad-index)
             (type sample scale-factor))
    (foreach-tick
      ;; Fill the zero-padded input buffer of the FFT
      (pconv-update-input fft-inbuf input-index in pad-index)
      ;; Update the output frame
      (pconv-update-frame frame output-index outbuf channels scale-factor)
      (when (>= input-index half-size)
        ;; FFT of the input
        (fft-execute (fft-plan fft) fft-inbuf fft-outbuf)
        ;; Copy the result of the FFT in the FDL
        (update-fdl fdl fft-outbuf fdl-head block-size)
        (dotimes (ch channels)
          ;; Multiply the partitions and mix the results
          (pconv-multiply-partitions ifft-inbuf fdl
                                     (mem-aref irdata :pointer ch)
                                     fdl-size fdl-head block-size partitions)
          ;; IFFT of the resultant complex spectrum
          (ifft-execute (ifft-plan ifft) ifft-inbuf ifft-outbuf)
          ;; Update the (interlaced) multichannel output buffer
          (pconv-update-output half-size ch channels outbuf
                               outbuf-half ifft-outbuf))
        (update-fdl-head fdl-head block-size fdl-last)
        (setf input-index 0 pad-index half-size output-index 0)))
    (if (< current-channel channels)
        (frame-ref frame current-channel)
        +sample-zero+)))
