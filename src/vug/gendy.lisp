;;; Copyright (c) 2013-2023 Tito Latini
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

;;; Implementation of the Génération Dynamique Stochastique (GENDYN),
;;; a dynamic stochastic approach to waveform synthesis conceived by
;;; Iannis Xenakis. See Formalized Music (1992, Stuyvesant, NY:
;;; Pendragon Press), pp. 246 - 254, 289 - 322.
;;; Based on Nick Collins's Gendy1 ugen (SuperCollider)

;;; The distributions are not the originals but rewritten by
;;; Nick Collins for SuperCollider to fix some numerical problems
(declaim (inline gendy-cauchy))
(defun gendy-cauchy (a)
  (* 0.1 (/ 1.0 a) (tan (* (atan (* 10.0 a))
                           (the (sample #.(sample -1) #.(sample 1))
                             (random-sample -1 1))))))

(declaim (inline gendy-logist))
(defun gendy-logist (a)
  (let* ((b (+ .5 (* .499 a)))
         (c (the sample (log (/ (- 1.0 b) b))))
         (r (+ (* (random-sample -.499 .499) a) 0.5)))
    (/ (the sample (log (/ (- 1.0 r) r))) c)))

(declaim (inline gendy-hyperbcos))
(defun gendy-hyperbcos (a)
  (let ((r (/ (tan (the maybe-limited-sample
                     (* a (random-sample 0 1.5692255))))
              (tan (the maybe-limited-sample (* 1.5692255 a))))))
    (- (* 2.0 (* (the sample (log (+ (* r 0.999) .001))) -0.1447648))
       1.0)))

(declaim (inline gendy-arcsine))
(defun gendy-arcsine (a)
  (/ (sin (the maybe-limited-sample (* (random-sample (- +half-pi+) +half-pi+) a)))
     (sin (the maybe-limited-sample (* 1.5707963 a)))))

(declaim (inline gendy-expon))
(defun gendy-expon (a)
  (- (* 2.0 (/ (the sample (log (- 1.0 (* (random-sample 0 0.999) a))))
               (the sample (log (- 1.0 (* 0.999 a))))))
     1.0))

(declaim (inline gendy-distribution))
(defun gendy-distribution (which a)
  (let ((a (clip a (sample 1d-4) (sample 1.0))))
    (cond ((= which 1) (gendy-cauchy a))
          ((= which 2) (gendy-logist a))
          ((= which 3) (gendy-hyperbcos a))
          ((= which 4) (gendy-arcsine a))
          ((= which 5) (gendy-expon a))
          ((= which 6) (- (* a (sample 2)) (sample 1))) ; external
          (t (random-sample -1 1)))))                   ; linear

(defmacro gendy-update-value (data index dist-var dist-value
                              dist-param scale min max)
  `(progn
     (setf ,dist-var (gendy-distribution ,dist-value ,dist-param))
     (setf (smp-ref ,data ,index)
           (mirror (+ (smp-ref ,data ,index) (* ,scale ,dist-var))
                   ,(sample min) ,(sample max)))))

(define-vug-macro gendy (amp-distr dur-distr amp-distr-param dur-distr-param
                         freq-min freq-max amp-scale dur-scale
                         &optional (max-points 12) (used-points max-points)
                         (interpolation :linear))
  "Dynamic stochastic approach to waveform synthesis conceived by
Iannis Xenakis.

The waveform is generated by USED-POINTS minus 1 segments and is
repeated in the time. The vertexes (control points) are moved
according to a stochastic action and they are limited within the
boundaries of a mirror formed by an amplitude barrier and a time
barrier.

AMP-DISTR is the probability distribution for the next perturbation of
the amplitude of a control point. The valid distributions are:

    0 - LINEAR
    1 - CAUCHY
    2 - LOGIST
    3 - HYPERBCOS
    4 - ARCSINE
    5 - EXPON
    6 - SINUS (external signal)

AMP-DISTR-PARAM is the parameter for the AMP-DISTR distribution.
Should be in the range of 0.0001 to 1 or a form that represents a
random number generator if AMP-DISTR is 6.

DUR-DISTR is the distribution for the perturbation of the current
inter control point duration. See AMP-DISTR for the valid distributions.

DUR-DISTR-PARAM is the parameter for the DUR-DISTR distribution.
Should be in the range of 0.0001 to 1 or a form that represents a
random number generator if DUR-DISTR is 6.

FREQ-MIN and FREQ-MAX are the minimum and maximum allowed frequency of
oscillation.

AMP-SCALE is the multiplier for the distribution's delta value for amplitude
(1.0 is full range).

DUR-SCALE is the multiplier for the distribution's delta value for duration.

MAX-POINTS minus 1 is the maximum number of segments.

INTERPOLATION is one of :LINEAR (default), :COS, :CUBIC or NIL."
  (if (constantp max-points)
      (with-gensyms (gendy)
        `(vuglet ((,gendy ((adist non-negative-fixnum)
                           (ddist non-negative-fixnum)
                           amp-distr-param dur-distr-param freq-min freq-max
                           amp-scale dur-scale (used-points fixnum))
                    (with ((max-points ,max-points)
                           (amps (make-frame max-points))
                           (durs (make-frame max-points))
                           (distrib +sample-zero+)
                           (freq-delta (- freq-max freq-min))
                           (index 0)
                           (points (clip used-points 1 max-points)))
                      (declare (type frame amps durs)
                               (type sample distrib freq-delta)
                               (type non-negative-fixnum index points))
                      (initialize
                        (dotimes (i max-points)
                          (setf (smp-ref amps i) (random-sample -1 1)
                                (smp-ref durs i) (random-sample 0 1))))
                      (interpolate
                        (tick
                         (prog1 (smp-ref amps index)
                           (when (>= (incf index) points) (setf index 0))
                           (gendy-update-value amps index distrib adist
                                               amp-distr-param amp-scale -1 1)
                           (gendy-update-value durs index distrib ddist
                                               dur-distr-param dur-scale 0 1)))
                        (* (+ freq-min (* freq-delta (smp-ref durs index)))
                           points)
                        ,interpolation))))
           (,gendy ,amp-distr ,dur-distr ,amp-distr-param ,dur-distr-param
                   ,freq-min ,freq-max ,amp-scale ,dur-scale ,used-points)))
      (incudine-error "MAX-POINTS of GENDY is not a constant")))
