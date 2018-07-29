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

(define-vug centroid ((abuf abuffer))
  "Compute the spectral centroid of the ABUFFER data using moments.

Overwrite the ABUFFER data with the result.

Example:

    (dsp! centroid-test ()
      (with ((fft (make-fft 1024 :window-size 512 :window-function (gen:hanning)))
             (abuf (make-abuffer fft))
             (result +sample-zero+))
        (declare (type sample result))
        (setf (fft-input fft)
              ;; Exponential growth from 1 to 30 harmonics in 20 seconds
              (buzz 440 .5 (sample->fixnum (expon 1 30 20 #'free))))
        (with-control-period ((sample->fixnum *sample-rate*))
          (setf result (* (centroid abuf) *sample-rate* 0.5))
          (nrt-msg info \"~D\" (sample->fixnum result)))
        (out (tick (fft-input fft)))))"
  (with-samples (num denom)
    (setf num +sample-zero+ denom +sample-zero+)
    (dofft-polar (i nbins ((compute-abuffer abuf)) ()
                  :result (if (zerop denom)
                              (sample 0.5)
                              (/ num (* (abuffer-nbins abuf) denom))))
      (incf num (* i mag0))
      (incf denom mag0))))

;;; Spectral flux.
;;; We can also use the half-wave rectifier function with L1-norm.
;;;
;;; References:
;;;
;;;   [1] Simon Dixon. Onset detection revisited. Proc. of the 9th
;;;   Int. Conference on Digital Audio Effects (DAFx-06), Montreal,
;;;   Canada, September 18-20, 2006.
;;;
;;;   [2] P. Masri. Computer modelling of sound for transformation and
;;;   synthesis of musical signal. Ph.D. dissertation, University of
;;;   Bristol, UK, 1996.
;;;
;;;   [3] C. Duxbury, M. Sandler, and M. Davies. A hybrid approach to
;;;   musical note onset detection. In Proc. Int. Conf. on Digital
;;;   Audio Effects (DAFx-02), Hamburg, Germany, 2002, pp. 33-38.
;;;
(define-vug-macro flux (abuf &optional half-wave-rectifier-p l1-norm-p)
  "Compute the spectral flux of the ABUFFER data.

If HALF-WAVE-RECTIFIER-P is T, use the half-wave rectifier function.

If L1-NORM-P is T, return the L1-norm of the spectral flux.

Overwrite the ABUFFER data with the result.

Examples:

    (dsp! flux-test ((buf buffer))
      (with ((fft (make-fft 2048 :window-function (gen:hamming)))
             (abuf (make-abuffer fft))
             (hop-size (sample->fixnum (* *sample-rate* 0.1)))
             (result +sample-zero+))
        (declare (type sample result))
        (setf (fft-input fft) (buffer-play buf 1 0 nil #'free))
        (with-control-period (hop-size)
          (setf result (flux abuf))
          (nrt-msg info \"~D\" (sample->fixnum result)))
        (out (tick (fft-input fft)))))

    (dsp! flux-rectified-test ((buf buffer) flux-threshold)
      (with ((fft (make-fft 2048 :window-function (gen:hamming)))
             (abuf (make-abuffer fft))
             (hop-size (sample->fixnum (* *sample-rate* 0.1)))
             ;; The result of FLUX is unnormalized
             (threshold (* flux-threshold (fft-size fft)))
             (result +sample-zero+))
        (declare (type sample threshold result))
        (setf (fft-input fft) (buffer-play buf 1 0 t #'identity))
        (with-control-period (hop-size)
          ;; Spectral flux with half-wave rectifier function
          ;; and L1-norm (Dixon DAFx-06)
          (setf result (sample (if (> (flux abuf t t) threshold) 100 0))))
        (out (tick (fft-input fft))
             (prog1 (ringz result 3000 .1)
               (unless (zerop result)
                 (setf result +sample-zero+))))))"
  (with-gensyms (flux)
    `(vuglet ((,flux ((abuf1 abuffer))
                (with-samples (diff result)
                  (with ((abuf-prev (make-abuffer (abuffer-link abuf1))))
                    (setf result +sample-zero+)
                    (dofft-polar (i nbins (abuf-prev (compute-abuffer abuf1)) ()
                                  :result ,(if l1-norm-p
                                               'result
                                               `(sqrt (the non-negative-sample
                                                           result))))
                      ;; We don't want the symbols MAG0 and MAG1 in INCUDINE.VUG
                      ;; package because they are interned during the expansion
                      ;; of DOFFT-POLAR. The following bindings are needed only
                      ;; within a VUG-MACRO that uses DOFFT.
                      ,(symbol-macrolet ((prev (intern "MAG0"))
                                         (curr (intern "MAG1")))
                         `(progn
                            (setf diff (- ,curr ,prev))
                            ,@(when (and half-wave-rectifier-p (not l1-norm-p))
                                `((setf diff (* (+ diff (abs diff)) 0.5))))
                            (setf ,prev ,curr)
                            (incf result
                                  ,(cond ((and half-wave-rectifier-p l1-norm-p)
                                          `(* (+ diff (abs diff)) 0.5))
                                         (l1-norm-p 'diff)
                                         (t `(* diff diff)))))))))))
       (,flux ,abuf))))

(define-vug spectral-rms ((abuf abuffer))
  "Compute the spectral RMS of the ABUFFER data.

Overwrite the ABUFFER data with the result.

Example:

    (dsp! spectral-rms-test ()
      (with ((fft (make-fft 1024 :window-function (gen:hanning)))
             (abuf (make-abuffer fft))
             (mult (/ (sample 1.0) (fft-size fft)))
             (rms +sample-zero+))
        (declare (type sample mult rms))
        (setf (fft-input fft) (audio-in 0))
        (with-control-period (1024)
          (setf rms (* (spectral-rms abuf) mult))
          (reduce-warnings (nrt-msg info \"~F\" rms)))))"
  (with-samples (rms)
    (setf rms +sample-zero+)
    (dofft-polar (i nbins ((compute-abuffer abuf)) ()
                  :result (sqrt (the non-negative-sample
                                  (/ rms (abuffer-nbins abuf)))))
      (incf rms (* mag0 mag0)))))

(define-vug rolloff ((abuf abuffer) percent)
  "Compute the spectral rolloff of the ABUFFER data.

Overwrite the ABUFFER data with the result.

Example:

    (dsp! rolloff-test (percent)
      (with ((fft (make-fft 1024 :window-function (gen:hanning)))
             (abuf (make-abuffer fft))
             (srdiv2 (* *sample-rate* 0.5))
             (perc (sample->fixnum (* percent 100)))
             (result +sample-zero+))
        (declare (type sample srdiv2 result))
        (setf (fft-input fft) (audio-in 0))
        (with-control-period (1024)
          (setf result (* (rolloff abuf percent) srdiv2))
          (nrt-msg info \"~D% rolloff: ~D\" perc (sample->fixnum result)))))"
  (with-samples (threshold result)
    (setf result +sample-zero+)
    (dofft-polar (i nbins ((compute-abuffer abuf)) ())
      (incf result mag0))
    (setf threshold (* result percent))
    (setf result +sample-zero+)
    (dofft-polar (i nbins ((compute-abuffer abuf)) ()
                  :result (/ result (abuffer-nbins abuf)))
      (incf result mag0)
      (when (>= result threshold)
        (setf result (sample i))
        (return)))))

(define-vug flatness ((abuf abuffer))
  "Compute the spectral flatness of the ABUFFER data.

Overwrite the ABUFFER data with the result.

Example:

    (define-vug crossfade (in1 in2 pos)
      (with-samples ((alpha (* +half-pi+ pos)))
        (+ (* (cos alpha) in1) (* (sin alpha) in2))))

    (dsp! flatness-test ()
      (with ((fft (make-fft 1024 :window-function (gen:hanning)))
             (abuf (make-abuffer fft))
             (result +sample-zero+))
        (declare (type sample result))
        (setf (fft-input fft)
              (crossfade (sine 1000 .5 0) (white-noise .1)
                         (line 0 1 8 #'free)))
        (with-control-period (1024)
          (setf result (flatness abuf))
          (nrt-msg info \"~D\" (sample->fixnum (* 100 result))))
        (out (tick (fft-input fft)))))"
  (with-samples (geometric-mean arithmetic-mean)
    (setf geometric-mean +sample-zero+
          arithmetic-mean +sample-zero+)
    (dofft-polar (i nbins ((compute-abuffer abuf)) ())
      ;; Sum of logarithms to avoid precision errors with the
      ;; floating point value of the magnitude.
      (incf geometric-mean (if (plusp mag0)
                               (log (the positive-sample mag0))
                               (log least-positive-sample)))
      (incf arithmetic-mean mag0))
    (with-samples ((r-nbins (/ (sample (abuffer-nbins abuf)))))
      ;; From log to linear scale
      (setf geometric-mean (exp (* geometric-mean r-nbins)))
      (if (zerop arithmetic-mean)
          +sample-zero+
          (/ geometric-mean (* arithmetic-mean r-nbins))))))
