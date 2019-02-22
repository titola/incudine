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

(in-package :incudine.analysis)

(deftype pvdata-type ()
  '(member :complex :magnitude-phase :magnitude-frequency))

(defstruct (pvbuffer (:include incudine-object)
                     (:constructor %make-pvbuffer)
                     (:copier nil))
  "PVbuffer type.

A PVBUFFER contains a sequence of spectral data."
  (data-ptr (null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (frames 0 :type non-negative-fixnum)
  (channels 1 :type (integer 0 #.(1+ incudine.util::maximum-channel-number)))
  (fft-size 1024 :type positive-fixnum)
  (scale-factor (sample 1) :type sample)
  (block-size 1026 :type positive-fixnum)
  (nbins 1 :type positive-fixnum)
  (srate incudine.util:*sample-rate* :type sample)
  (block-data-type :complex :type pvdata-type)
  (window-buffer (null-pointer) :type foreign-pointer)
  (window-size 1024 :type positive-fixnum)
  (window-function #'rectangular-window :type (or function null))
  (hop-size 1024 :type positive-fixnum)
  ;; freq = k * delta_phase + m * nbin
  ;; k = srate / (2pi * hopsize)
  (delta-phase-mult +sample-zero+ :type sample)
  ;; m = srate / fftsize
  (central-frequency-mult +sample-zero+ :type sample)
  ;; Phase offset for phase unwrapping before the conversion to
  ;; magnitude-frequency:
  ;;                                     _.------ mult -------._
  ;;   phase_unwrap = ph1 - ph0 - nbin * 2pi * hopsize / fftsize
  ;;
  ;; The alternative is a circular shift during the FFT but it is
  ;; preferable the phase shift just for the data type magnitude-frequency.
  (phase-offset-mult +sample-zero+ :type sample)
  (normalized-p nil :type boolean)
  ;; Whether to use a zero phase window in STFT.
  (zero-phase-window-p nil :type boolean)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(setf
  (documentation 'pvbuffer-size 'function)
  "Return the PVbuffer size."
  (documentation 'pvbuffer-frames 'function)
  "Return the number of the PVbuffer spectral frames."
  (documentation 'pvbuffer-channels 'function)
  "Return the number of the PVbuffer channels."
  (documentation 'pvbuffer-fft-size 'function)
  "Return the FFT size used to compute the spectral data of the PVbuffer."
  (documentation 'pvbuffer-scale-factor 'function)
  "Return the PVbuffer scale factor. It is one if the PVbuffer data are
normalized."
  (documentation 'pvbuffer-block-size 'function)
  "Return the PVbuffer block size."
  (documentation 'pvbuffer-normalized-p 'function)
  "Whether the PVbuffer data are normalized.")

(define-constant +pvbuffer-pool-initial-size+ 20)

(defvar *pvbuffer-pool*
  (make-incudine-object-pool +pvbuffer-pool-initial-size+ #'%make-pvbuffer nil))
(declaim (type incudine-object-pool *pvbuffer-pool*))

(defvar *rt-pvbuffer-pool*
  (make-incudine-object-pool +pvbuffer-pool-initial-size+ #'%make-pvbuffer t))
(declaim (type incudine-object-pool *rt-pvbuffer-pool*))

(defun make-pvbuffer (frames fft-size &key (channels 1)
                      (sample-rate incudine.util:*sample-rate*)
                      (data-type :complex) (window-size fft-size)
                      (window-function *fft-default-window-function*)
                      (hop-size (ash window-size -2)) normalized-p
                      (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new PVBUFFER structure with the specified FRAMES
and FFT-SIZE.

The number of CHANNELS defaults to 1.

The SAMPLE-RATE of the analysed data is INCUDINE.UTIL:*SAMPLE-RATE* by
default.

DATA-TYPE is one of :COMPLEX (default), :MAGNITUDE-PHASE or
:MAGNITUDE-FREQUENCY.

WINDOW-SIZE is the size of the analysis window and defaults to FFT-SIZE.

WINDOW-FUNCTION is NIL or a function of two arguments called to fill the
window buffer. The function arguments are the foreign pointer to the
data window of type SAMPLE and the window size respectively.
WINDOW-FUNCTION defaults to *FFT-DEFAULT-WINDOW-FUNCTION*.

If the PVBUFFER is used to store the STFT frames, HOP-SIZE is the STFT
frame offset and defaults to a quarter of WINDOW-SIZE.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type non-negative-fixnum frames)
           (type positive-fixnum channels fft-size window-size hop-size)
           (type pvdata-type data-type)
           (type alexandria:positive-real sample-rate)
           (type (or function null) window-function)
           (type boolean real-time-p))
  (let ((data nil)
        (window-data nil)
        (block-size (* 2 (1+ (ash fft-size -1))))
        (rt-p (and real-time-p incudine.util:*allow-rt-memory-pool-p*)))
    (multiple-value-bind (obj free-fn pool)
        (if rt-p
            (values (incudine.util::alloc-rt-object *rt-pvbuffer-pool*)
                    #'safe-foreign-rt-free
                    *rt-pvbuffer-pool*)
            (values (incudine.util::alloc-object *pvbuffer-pool*)
                    #'foreign-free
                    *pvbuffer-pool*))
      (declare (type pvbuffer obj) (type incudine-object-pool pool))
      (handler-case
          (let ((size (* block-size frames))
                (srate (sample sample-rate))
                (k (* +twopi+ hop-size)))
            (declare #.*standard-optimize-settings*)
            (when (> window-size fft-size)
              (setf window-size fft-size))
            (setf data
                  (alloc-multi-channel-data channels size :real-time-p rt-p))
            (reduce-warnings
              (setf window-data
                    (funcall (if rt-p #'foreign-rt-alloc #'cffi:foreign-alloc)
                             'sample :count fft-size)))
            (when window-function
              (fill-window-buffer window-data window-function window-size))
            (setf (pvbuffer-data-ptr obj) data
                  (pvbuffer-size obj) size
                  (pvbuffer-frames obj) frames
                  (pvbuffer-channels obj) channels
                  (pvbuffer-fft-size obj) fft-size
                  (pvbuffer-scale-factor obj) (if normalized-p
                                                  (sample 1)
                                                  (/ (sample 1) fft-size))
                  (pvbuffer-block-size obj) block-size
                  (pvbuffer-nbins obj) (ash block-size -1)
                  (pvbuffer-srate obj) srate
                  (pvbuffer-block-data-type obj) data-type
                  (pvbuffer-window-buffer obj) window-data
                  (pvbuffer-window-size obj) window-size
                  (pvbuffer-window-function obj) window-function
                  (pvbuffer-hop-size obj) hop-size
                  (pvbuffer-normalized-p obj) normalized-p
                  (pvbuffer-delta-phase-mult obj) (/ srate k)
                  (pvbuffer-central-frequency-mult obj) (/ srate fft-size)
                  (pvbuffer-phase-offset-mult obj) (/ k fft-size)
                  (pvbuffer-real-time-p obj) rt-p
                  (pvbuffer-foreign-free obj) free-fn)
            (when (eq data-type :magnitude-frequency)
              (dotimes (chan channels)
                (pv-polar-to-frequency obj nil chan))))
        (condition (c)
          (if data (free-multi-channel-data data channels free-fn))
          (if window-data (funcall free-fn window-data))
          (incudine-object-pool-expand pool 1)
          (error c)))
      (incudine-finalize obj
        (lambda ()
          (free-multi-channel-data data channels free-fn)
          (foreign-free window-data)
          (incudine-object-pool-expand pool 1))))))

(defmethod incudine:free ((obj pvbuffer))
  (let ((data (pvbuffer-data-ptr obj)))
    (unless (null-pointer-p data)
      (free-multi-channel-data data (pvbuffer-channels obj)
                               (pvbuffer-foreign-free obj))
      (unless (null-pointer-p (pvbuffer-window-buffer obj))
        (funcall (pvbuffer-foreign-free obj)
                 (pvbuffer-window-buffer obj)))
      (incudine-cancel-finalization obj)
      (setf (pvbuffer-data-ptr obj) (null-pointer))
      (if (pvbuffer-real-time-p obj)
          (incudine.util::free-rt-object obj *rt-pvbuffer-pool*)
          (incudine.util::free-object obj *pvbuffer-pool*))
      (incudine.util:nrt-msg debug "Free ~A" (type-of obj)))
    (values)))

(defmethod incudine:free-p ((obj pvbuffer))
  (null-pointer-p (pvbuffer-data-ptr obj)))

(defmethod print-object ((obj pvbuffer) stream)
  (multiple-value-bind (size frames channels block-size)
      (if (incudine:free-p obj)
          (values 0 0 1 0)
          (values (pvbuffer-size obj) (pvbuffer-frames obj)
                  (pvbuffer-channels obj) (pvbuffer-block-size obj)))
    (format stream "#<PVBUFFER :SIZE ~D :FRAMES ~D :CHANNELS ~D :BLOCK-SIZE ~D>"
            size frames channels block-size)))

(defun pvbuffer-data (instance &optional (frame 0) (channel 0))
  "Return the foreign pointer to the data FRAME of a PVbuffer CHANNEL.

FRAME and CHANNEL default to zero.

No bounds checking."
  (cffi:inc-pointer
    (cffi:mem-aref (pvbuffer-data-ptr instance) :pointer channel)
    (the non-negative-fixnum
      (* (pvbuffer-block-size instance) frame +foreign-sample-size+))))

(defmacro pvbuffer-loop ((frame-var obj &key reverse-p) &body body)
  `(loop for ,frame-var of-type
             ,@(if reverse-p
                   `(fixnum from (1- (pvbuffer-frames ,obj)) downto 0)
                   `(non-negative-fixnum below (pvbuffer-frames ,obj)))
         ,@body))

(declaim (inline pv-phase-rewrap))
(defun pv-phase-rewrap (x)
  (let ((res (incudine.external::%fmod (+ x pi) (- +twopi+))))
    (if (plusp res)
        (- res pi)
        (+ res pi))))

(defun pv-complex-to-polar (pvbuf &optional frame (channel 0))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (if frame
      (complex-to-polar (pvbuffer-data pvbuf frame channel)
                        (pvbuffer-nbins pvbuf))
      (pvbuffer-loop (frame pvbuf) do
        (pv-complex-to-polar pvbuf frame channel)))
  pvbuf)

(defun pv-polar-to-complex (pvbuf &optional frame (channel 0))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (if frame
      (polar-to-complex (pvbuffer-data pvbuf frame channel)
                        (pvbuffer-nbins pvbuf))
      (pvbuffer-loop (frame pvbuf) do
        (pv-polar-to-complex pvbuf frame channel)))
  pvbuf)

(defun pv-polar-to-frequency (pvbuf &optional frame (channel 0)
                              (zero-phase-window-p nil zpwin-p))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (let ((zero-phase-window-p (if zpwin-p
                                 zero-phase-window-p
                                 (pvbuffer-zero-phase-window-p pvbuf))))
    (if frame
        (let ((ptr (pvbuffer-data pvbuf frame channel))
              (delta-phase-mult (pvbuffer-delta-phase-mult pvbuf))
              (central-frequency-mult (pvbuffer-central-frequency-mult pvbuf)))
          (loop for nbin of-type non-negative-fixnum from 0
                for i of-type non-negative-fixnum
                      from 1 below (pvbuffer-block-size pvbuf) by 2
                with ptr-prev = (and (> frame 0)
                                     (pvbuffer-data
                                       pvbuf (1- frame) channel))
                do (when ptr-prev
                     (decf (smp-ref ptr i)
                           (+ (smp-ref ptr-prev i)
                              (* nbin (pvbuffer-phase-offset-mult pvbuf)))))
                   (setf (smp-ref ptr i) (pv-phase-rewrap (smp-ref ptr i)))
                   (if (= nbin 0)
                       (unless (= (smp-ref ptr i) 0)
                         ;; Generally, the frequency preserves the sign to inform
                         ;; about the phase inversion, but the DC frequency is
                         ;; always zero, therefore that information is moved to
                         ;; the amplitude by changing the sign. A conversion from
                         ;; magnitude-frequency to magnitude-phase restores the
                         ;; phase to 0 or pi.
                         (setf (smp-ref ptr i) (sample 0))
                         (setf (smp-ref ptr (1- i)) (- (smp-ref ptr (1- i)))))
                       (setf (smp-ref ptr i)
                             (+ (* delta-phase-mult (smp-ref ptr i))
                                ;; If ZERO-PHASE-WINDOW-P is NIL,
                                ;; central_freq +/- x is shifted to
                                ;; initial_freq + 2x.
                                (* nbin central-frequency-mult))))))
        (pvbuffer-loop (frame pvbuf :reverse-p t) do
          (pv-polar-to-frequency pvbuf frame channel zero-phase-window-p))))
  pvbuf)

(defun pv-frequency-to-polar (pvbuf &optional frame (channel 0))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (if frame
      (let ((ptr (pvbuffer-data pvbuf frame channel))
            (k (/ (pvbuffer-delta-phase-mult pvbuf)))
            (central-frequency-mult (pvbuffer-central-frequency-mult pvbuf)))
        (loop for nbin of-type non-negative-fixnum from 0
              for i of-type non-negative-fixnum
                    from 1 below (pvbuffer-block-size pvbuf) by 2
              with ptr-prev = (and (> frame 0)
                                   ;; Pointer to the previous computed phase.
                                   (pvbuffer-data
                                     pvbuf (1- frame) channel))
              do (cond ((= nbin 0)
                        ;; The conversion to magnitude-frequency moves
                        ;; the information about the phase inversion to
                        ;; the amplitude.
                        (let ((inv-phase-p (minusp (smp-ref ptr (1- i)))))
                          (when inv-phase-p
                            ;; Restore the sign of the amplitude value.
                            (setf (smp-ref ptr (1- i)) (- (smp-ref ptr (1- i)))))
                          (setf (smp-ref ptr i)
                                (if ptr-prev
                                    (if inv-phase-p
                                        (if (zerop (smp-ref ptr-prev i))
                                            pi
                                            +sample-zero+)
                                        (smp-ref ptr-prev i))
                                    (if inv-phase-p pi +sample-zero+)))))
                       (t
                        (setf (smp-ref ptr i)
                              (* k (- (smp-ref ptr i)
                                      (* central-frequency-mult nbin))))
                        (when ptr-prev
                          (incf (smp-ref ptr i)
                                (+ (smp-ref ptr-prev i)
                                   (* nbin
                                      (pvbuffer-phase-offset-mult pvbuf)))))))))
      (pvbuffer-loop (frame pvbuf :reverse-p nil) do
        (pv-frequency-to-polar pvbuf frame channel)))
  pvbuf)

(defun pv-complex-to-frequency (pvbuf &optional frame (channel 0))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (if frame
      (error 'simple-type-error
        :format-control
          "Cannot convert the data type of a single PVbuffer frame~%~
           from COMPLEX to MAGNITUDE-FREQUENCY.")
      (let ((frames (pvbuffer-frames pvbuf))
            (zero-phase-window-p (pvbuffer-zero-phase-window-p pvbuf)))
        (when (> frames 1)
          (complex-to-polar
            (pvbuffer-data pvbuf (- frames 1) channel)
            (pvbuffer-nbins pvbuf))
          (loop for frame of-type fixnum from (1- frames) downto 1 do
                 (complex-to-polar
                   (pvbuffer-data pvbuf (1- frame) channel)
                   (pvbuffer-nbins pvbuf))
                 (pv-polar-to-frequency pvbuf frame channel
                                        zero-phase-window-p)))
        (pv-polar-to-frequency pvbuf 0 channel zero-phase-window-p)
        pvbuf)))

(defun pv-frequency-to-complex (pvbuf &optional frame (channel 0))
  (declare (type pvbuffer pvbuf)
           (type (or null non-negative-fixnum) frame)
           (type non-negative-fixnum channel))
  (if frame
      (error 'simple-type-error
        :format-control
          "Cannot convert the data type of a single PVbuffer frame~%~
           from MAGNITUDE-FREQUENCY to COMPLEX.")
      (let* ((frames (pvbuffer-frames pvbuf))
             (last-frame (1- frames)))
        (pv-frequency-to-polar pvbuf 0 channel)
        (when (> frames 1)
          (loop for frame of-type non-negative-fixnum below last-frame do
                 (pv-frequency-to-polar pvbuf (1+ frame) channel)
                 (polar-to-complex (pvbuffer-data pvbuf frame)
                                   (pvbuffer-nbins pvbuf))))
        (polar-to-complex (pvbuffer-data pvbuf last-frame)
                          (pvbuffer-nbins pvbuf))
        pvbuf)))

(defun pv-data-type-convert-function (from to)
  (declare (type pvdata-type from to))
  (unless (eq from to)
    (flet ((conv-p (itype otype)
             (and (eq from itype) (eq to otype))))
      (cond ((conv-p :complex :magnitude-phase) #'pv-complex-to-polar)
            ((conv-p :magnitude-phase :complex) #'pv-polar-to-complex)
            ((conv-p :complex :magnitude-frequency) #'pv-complex-to-frequency)
            ((conv-p :magnitude-frequency :complex) #'pv-frequency-to-complex)
            ((conv-p :magnitude-phase :magnitude-frequency)
             #'pv-polar-to-frequency)
            ((conv-p :magnitude-frequency :magnitude-phase)
             #'pv-frequency-to-polar)))))

(defun fill-pvbuffer (instance frame obj &key (channel 0))
  "If the PVbuffer data type is not :MAGNITUDE-FREQUENCY and OBJ is of
type FFT or ABUFFER, fill the data FRAME of the PVbuffer CHANNEL with
the FFT analysis.

CHANNEL defaults to 0.

Example:

     (defvar *pvb* (make-pvbuffer 10 1024 :window-size 512))
     (defvar *fft* (make-fft-from-pvbuffer *pvb*))

     (dotimes (frame 10)
       (dotimes (i 1024)
         (setf (fft-input *fft*) (random 1d0)))
       ;; zero-phase FFT window.
       (circular-shift *fft* -256)
       (fill-pvbuffer *pvb* frame (compute-fft *fft*)))

     ;; Convert data type from complex to magnitude-frequency.
     ;; (setf (pvbuffer-data-type *pvb*) :magnitude-frequency)"
  (declare (type pvbuffer instance)
           (type non-negative-fixnum frame channel)
           (type (or fft abuffer) obj))
  (if (eq (pvbuffer-block-data-type instance) :magnitude-frequency)
      (error 'simple-type-error
        :format-control
          "(SETF PVBUFFER-DATA) doesn't work with data type :MAGNITUDE-FREQUENCY")
      (multiple-value-bind (ptr data-type scale-p)
          (typecase obj
            (fft (values (fft-output-buffer obj)
                         (fft-output-data-type obj)
                         (pvbuffer-normalized-p instance)))
            (abuffer (values (abuffer-data obj)
                             (abuffer-data-type obj)
                             (not (eq (abuffer-normalized-p obj)
                                      (pvbuffer-normalized-p instance))))))
        (let ((frame-data (cffi:inc-pointer
                            (cffi:mem-aref (pvbuffer-data-ptr instance)
                                           :pointer channel)
                            (the non-negative-fixnum
                              (* (pvbuffer-block-size instance)
                                 frame +foreign-sample-size+)))))
          (if scale-p
              (loop for i of-type non-negative-fixnum
                          below (pvbuffer-block-size instance)
                          by (if (eq :complex data-type) 1 2)
                    with scl = (if (pvbuffer-normalized-p instance)
                                   (if (typep obj 'fft)
                                       (fft-scale-factor obj)
                                       (abuffer-scale-factor obj))
                                   (sample (pvbuffer-fft-size instance)))
                    do (setf (smp-ref frame-data i) (* (smp-ref ptr i) scl)))
              (foreign-copy-samples
                frame-data ptr (pvbuffer-block-size instance)))
          (unless (eq (pvbuffer-block-data-type instance) data-type)
            (funcall (the function
                       (pv-data-type-convert-function
                         data-type (pvbuffer-block-data-type instance)))
                     instance frame channel))
          obj))))

(defun copy-pvbuffer-data (instance destination &optional (frame 0) (channel 0))
  "If the PVbuffer data type is not :MAGNITUDE-FREQUENCY, copy the data FRAME
of a PVbuffer CHANNEL to DESTINATION and return the PVbuffer instance.

DESTINATION is of type IFFT or ABUFFER."
  (declare (type pvbuffer instance)
           (type (or ifft abuffer) destination)
           (type non-negative-fixnum frame channel))
  (if (eq (pvbuffer-block-data-type instance) :magnitude-frequency)
      (error 'simple-type-error
        :format-control
          "COPY-PVBUFFER-DATA doesn't work with data type :MAGNITUDE-FREQUENCY")
      (cond ((typep destination 'ifft)
             (loop for i below (ifft-input-buffer-size destination)
                   with dest = (ifft-input-buffer destination)
                   with src = (pvbuffer-data instance frame channel)
                   with scl = (pvbuffer-scale-factor instance)
                   with scale-p = (not (pvbuffer-normalized-p instance)) do
                     (setf (smp-ref dest i)
                           (if scale-p
                               (* scl (smp-ref src i))
                               (smp-ref src i))))
             (setf (ifft-input-changed-p destination) t))
            (t
             (foreign-copy-samples
               (abuffer-data destination)
               (pvbuffer-data instance frame channel)
               (abuffer-size destination))
             (setf (abuffer-data-type destination)
                   (pvbuffer-data-type instance))
             (setf (abuffer-normalized-p destination)
                   (pvbuffer-normalized-p instance))
             (setf (abuffer-time destination) (now)))))
  instance)

(defun pvbuffer-data-type (obj)
  "Return the PVbuffer data type. It is one of :COMPLEX (default),
:MAGNITUDE-PHASE or :MAGNITUDE-FREQUENCY. Setfable."
  (pvbuffer-block-data-type obj))

(defun set-pvbuffer-data-type (obj value)
  (declare (type pvbuffer obj) (type pvdata-type value))
  (unless (eq (pvbuffer-block-data-type obj) value)
    (let ((func (pv-data-type-convert-function
                  (pvbuffer-block-data-type obj) value)))
      (declare (type function func))
      (dotimes (chan (pvbuffer-channels obj))
        (funcall func obj nil chan)))
    (setf (pvbuffer-block-data-type obj) value))
  value)

(defsetf pvbuffer-data-type set-pvbuffer-data-type)

(defun normalize-pvbuffer (obj)
  "Multiply the magnitudes of the unnormalized PVbuffer data by 1/fftsize."
  (unless (pvbuffer-normalized-p obj)
    (let ((scl (pvbuffer-scale-factor obj)))
      (loop for chan below (pvbuffer-channels obj) do
           (loop for i below (pvbuffer-size obj)
                       by (if (eq (pvbuffer-block-data-type obj) :complex) 1 2)
                 with data = (cffi:mem-aref (pvbuffer-data-ptr obj)
                                            :pointer chan)
                 do (setf (smp-ref data i) (* (smp-ref data i) scl))))
      (setf (pvbuffer-normalized-p obj) t)
      (setf (pvbuffer-scale-factor obj) (sample 1))))
  obj)

(defun pvbuffer-window (instance)
  "Return the foreign pointer to the sample values of the analysis
window data and window size of the PVbuffer."
  (values (pvbuffer-window-buffer instance) (pvbuffer-window-size instance)))

(defun pvbuffer-sample-rate (instance)
  "Return the PVbuffer sample rate. Setfable."
  (pvbuffer-srate instance))

(defun set-pvbuffer-sample-rate (instance value)
  (setf (pvbuffer-srate instance) (sample value))
  (setf (pvbuffer-delta-phase-mult instance)
        (/ (pvbuffer-srate instance)
           (* +twopi+ (pvbuffer-hop-size instance))))
  (setf (pvbuffer-central-frequency-mult instance)
        (/ (pvbuffer-srate instance)
           (pvbuffer-fft-size instance)))
  value)

(defsetf pvbuffer-sample-rate set-pvbuffer-sample-rate)

(defmethod window-function ((obj pvbuffer))
  (pvbuffer-window-function obj))

(defmethod (setf window-function) ((fn function) (obj pvbuffer))
  (fill-window-buffer (pvbuffer-window-buffer obj) fn
                      (pvbuffer-window-size obj))
  (setf (pvbuffer-window-function obj) fn))

(defmethod (setf window-function) ((fn null) (obj pvbuffer))
  (setf (pvbuffer-window-function obj) nil))

(defmethod window-size ((obj pvbuffer))
  (pvbuffer-window-size obj))

(defmethod (setf window-size) (size (obj pvbuffer))
  (declare (type positive-fixnum size))
  (when (> size (pvbuffer-fft-size obj))
    (setf size (pvbuffer-fft-size obj)))
  (unless (= size (pvbuffer-window-size obj))
    (setf (pvbuffer-window-size obj) size)
    (when (pvbuffer-window-function obj)
      (fill-window-buffer (pvbuffer-window-buffer obj)
                          (the function (pvbuffer-window-function obj))
                          size)))
  size)

(defmethod hop-size ((obj pvbuffer))
  (pvbuffer-hop-size obj))

(defmethod (setf hop-size) (size (obj pvbuffer))
  (let ((k (* +twopi+ size)))
    (setf (pvbuffer-hop-size obj) size)
    (setf (pvbuffer-delta-phase-mult obj) (/ (pvbuffer-srate obj) k))
    (setf (pvbuffer-phase-offset-mult obj) (/ k (pvbuffer-fft-size obj)))
    size))

(defmacro %make-fft-from-pvbuffer (pvbuf constructor flags real-time-p)
  (with-gensyms (fft)
    `(incudine.util::with-struct-slots
         ((fft-size window-buffer window-size window-function)
          ,pvbuf PVBUFFER "INCUDINE.ANALYSIS")
       (let ((,fft (,constructor fft-size :window-size window-size
                                 :window-function window-function :flags ,flags
                                 :real-time-p ,real-time-p)))
         (unless window-function
           (handler-case
               (foreign-copy-samples
                 (,(if (eq constructor 'make-fft)
                       'fft-window-buffer
                       'ifft-window-buffer)
                   ,fft)
                 window-buffer window-size)
             (condition (c) (free ,fft) (error c))))
         ,fft))))

(defun make-fft-from-pvbuffer (instance &key flags
                               (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new FFT structure to fill the PVbuffer frames.

The argument FLAGS for the FFT planner is usually +FFT-PLAN-OPTIMAL+,
+FFT-PLAN-BEST+ or +FFT-PLAN-FAST+. If a cached FFT-PLAN with the same size
already exists, return it if the current thread is the real-time thread,
otherwise recompute the plan if FLAGS is non-NIL or the cached FFT-PLAN is
not the best. Without a cached FFT-PLAN, FLAGS defaults to +FFT-PLAN-FAST+.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (%make-fft-from-pvbuffer instance make-fft flags real-time-p))

(defun make-ifft-from-pvbuffer (instance &key flags
                                (real-time-p (incudine.util:allow-rt-memory-p)))
  "Create and return a new IFFT structure to transform the PVbuffer data.

The argument FLAGS for the FFT planner is usually +FFT-PLAN-OPTIMAL+,
+FFT-PLAN-BEST+ or +FFT-PLAN-FAST+. If a cached FFT-PLAN with the same size
already exists, return it if the current thread is the real-time thread,
otherwise recompute the plan if FLAGS is non-NIL or the cached FFT-PLAN is
not the best. Without a cached FFT-PLAN, FLAGS defaults to +FFT-PLAN-FAST+.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (%make-fft-from-pvbuffer instance make-ifft flags real-time-p))

(defun stft-loop (pvbuf obj start frames)
  (declare (type pvbuffer pvbuf)
           (type (or incudine:buffer soundfile:stream) obj)
           (type non-negative-fixnum start frames))
  (let* ((block-size (pvbuffer-block-size pvbuf))
         (channels (pvbuffer-channels pvbuf))
         (hop-size (pvbuffer-hop-size pvbuf))
         (normalized-p (pvbuffer-normalized-p pvbuf))
         (zero-phase-window-p (pvbuffer-zero-phase-window-p pvbuf))
         (window-half-size (- (ash (pvbuffer-window-size pvbuf) -1)))
         (step (* +foreign-sample-size+ block-size))
         (fft (make-fft-from-pvbuffer pvbuf)))
    (declare (type non-negative-fixnum block-size step)
             (type fixnum window-half-size)
             (optimize speed (safety 0)))
    (unwind-protect
         (loop for chan below channels
               with j of-type fixnum = 0 do
                 (if (incudine:buffer-p obj)
                     (setf j (* channels (1- start)))
                     (setf (soundfile:position obj) start))
                 (loop for frame below frames do
                         (incudine.util:msg debug
                           "[STFT] channel ~D, frame ~D" chan frame)
                      (loop repeat hop-size do
                              (setf (fft-input fft)
                                    (if (incudine:buffer-p obj)
                                        (incudine:buffer-value obj
                                          (the non-negative-fixnum
                                            (+ (the non-negative-fixnum
                                                 (incf j channels)) chan)))
                                        (soundfile:read-next obj chan))))
                      (when zero-phase-window-p
                        (circular-shift fft window-half-size))
                      (compute-fft fft)
                      (loop for i below block-size do
                              (setf (smp-ref (cffi:inc-pointer
                                               (cffi:mem-aref
                                                 (pvbuffer-data-ptr pvbuf)
                                                 :pointer chan)
                                               (the non-negative-fixnum
                                                 (* frame step)))
                                             i)
                                    (if normalized-p
                                        (* (pvbuffer-scale-factor pvbuf)
                                           (smp-ref (fft-output-buffer fft) i))
                                        (smp-ref (fft-output-buffer fft) i))))))
      (free fft))
    pvbuf))

(defun stft-from-soundfile (path fft-size window-size window-function hop-size
                            start frames normalized-p zero-phase-window-p)
  (declare (type (or string pathname) path)
           (type positive-fixnum fft-size window-size hop-size)
           (type function window-function)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) frames)
           (type boolean normalized-p zero-phase-window-p))
  (soundfile:with-open-soundfile (sf path)
    (let* ((frames (ceiling (/ (if frames
                                   (min frames (soundfile:frames sf))
                                   (soundfile:frames sf))
                               hop-size)))
           (pvbuf (make-pvbuffer frames fft-size
                    :channels (soundfile:channels sf)
                    :sample-rate (soundfile:sample-rate sf)
                    :window-size window-size
                    :window-function window-function
                    :hop-size hop-size
                    :normalized-p normalized-p)))
      (handler-case
          (progn
            (setf (pvbuffer-zero-phase-window-p pvbuf) zero-phase-window-p)
            (stft-loop pvbuf sf start frames))
        (condition (c) (free pvbuf) (error c))))))

(defun stft-from-buffer (buffer fft-size window-size window-function hop-size
                         start frames normalized-p zero-phase-window-p)
  (declare (type incudine:buffer buffer)
           (type positive-fixnum fft-size window-size hop-size)
           (type function window-function)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) frames)
           (type boolean normalized-p zero-phase-window-p))
  (let* ((frames (ceiling (/ (if frames
                                 (min frames (incudine:buffer-frames buffer))
                                 (incudine:buffer-frames buffer))
                             hop-size)))
         (pvbuf (make-pvbuffer frames fft-size
                  :channels (incudine:buffer-channels buffer)
                  :sample-rate (incudine:buffer-sample-rate buffer)
                  :window-size window-size
                  :window-function window-function
                  :hop-size hop-size
                  :normalized-p normalized-p)))
    (handler-case
        (progn
          (setf (pvbuffer-zero-phase-window-p pvbuf) zero-phase-window-p)
          (stft-loop pvbuf buffer start frames))
      (condition (c) (free pvbuf) (error c)))))

(defun stft (input fft-size &key (window-size (ash fft-size -1))
             (window-function *fft-default-window-function*)
             (hop-size (ash window-size -2)) (start 0) frames
             (normalized-p t) zero-phase-window-p)
  "Create and return a new PVbuffer structure with spectral data
obtained from the Short-Time Fourier Transform of a sound file or
BUFFER data with the specified FFT-SIZE.

INPUT is of type STRING, PATHNAME or BUFFER.

WINDOW-SIZE defaults to half FFT-SIZE.

WINDOW-FUNCTION is a function of two arguments called to fill the
FFT data window. The function arguments are the foreign pointer to
the data window of type SAMPLE and the window size respectively.
WINDOW-FUNCTION defaults to *FFT-DEFAULT-WINDOW-FUNCTION*.

HOP-SIZE is the STFT frame offset and defaults to a quarter of
WINDOW-SIZE.

START and FRAMES mark the beginning position in frames and the number
of frames of INPUT, respectively.

If NORMALIZED-P is T (default), the FFT analysis data are normalized.

If ZERO-PHASE-WINDOW-P is T, the FFT windows are zero-phase."
  (declare (type (or string pathname incudine:buffer) input)
           (type positive-fixnum fft-size window-size hop-size)
           (type function window-function)
           (type non-negative-fixnum start)
           (type (or null positive-fixnum) frames)
           (type boolean normalized-p zero-phase-window-p))
  (funcall (if (incudine:buffer-p input)
               #'stft-from-buffer
               #'stft-from-soundfile)
           input fft-size window-size window-function hop-size
           start frames normalized-p zero-phase-window-p))

(defun make-part-convolve-buffer (buf partsize &key (start 0) frames)
  "Create and return a new PVbuffer structure with spectral data
obtained from a sequence of FFT's of the BUFFER structure BUF
partitioned with partition size PARTSIZE. This PVbuffer works with
PART-CONVOLVE, a VUG to compute the partitioned convolution between a
signal and a multi-channel impulse response.

The optional keywords START and FRAMES mark the beginning position in
frames and the number of frames of the buffer, respectively."
  (declare (type incudine:buffer buf) (type positive-fixnum partsize)
           (type non-negative-fixnum start)
           (type (or null positive-fixnum) frames))
  (let ((fft-size (* 2 partsize)))
    (stft-from-buffer buf fft-size partsize #'rectangular-window partsize
                      start frames nil nil)))
