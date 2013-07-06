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

(in-package :incudine.vug)

;;; Moog Ladder Filter; see Antti Huovilainen, "Non-Linear Digital
;;; Implementation of the Moog Ladder Filter", DAFx'04, Naples, Italy.
;;;
;;; Based on Victor Lazzarini's moogladder opcode (Csound)

(define-constant +r-moogladder-thermal+ 40000.0d0)
;;; Transistor thermal voltage
(define-constant +moogladder-thermal+ (/ 1.0d0 +r-moogladder-thermal+))

(define-vug moogladder (in freq resonance)
  (with-samples (x0 y0 y1 stage0 stage1 stage2 stage3 tanh-0 tanh-1 tanh-2)
    (with-samples ((f (* freq *sample-duration*))
                   (ff (* f f))
                   (fff (* ff f))
                   (f/2 (* f 0.5))
                   (freq-correction (- (+ (* 1.8730 fff) (* 0.4955 ff) 0.9988)
                                       (* 0.6490 f)))
                   (amp-correction (+ (* -3.9364 ff) (* 1.8409 f) 0.9968))
                   ;; Filter tuning
                   (tune (* (- 1.0d0 (exp (- (* +twopi+ f/2 freq-correction))))
                            +r-moogladder-thermal+))
                   (res4 (if (plusp resonance)
                             (* 4.0d0 resonance amp-correction)
                             0.0d0)))
      (macrolet ((with-oversampling ((n) &body body)
                   `(progn ,@(loop repeat n append body))))
        (with-oversampling (2)
          (setf x0 (- in (* res4 y0)))
          (incf stage0 (* tune (- (tanh (* x0 +moogladder-thermal+)) tanh-0)))
          (setf tanh-0 (tanh (* stage0 +moogladder-thermal+)))
          (incf stage1 (* tune (- tanh-0 tanh-1)))
          (setf tanh-1 (tanh (* stage1 +moogladder-thermal+)))
          (incf stage2 (* tune (- tanh-1 tanh-2)))
          (setf tanh-2 (tanh (* stage2 +moogladder-thermal+)))
          (incf stage3 (* tune (- tanh-1 (tanh (* stage3 +moogladder-thermal+)))))
          ;; 1/2-sample delay for phase compensation
          (setf y0 (* (+ stage3 y1) 0.5))
          (setf y1 stage3))
        y0))))

;;; A discrete-time version of the Moog VCF; see Federico Fontana,
;;; "Preserving the structure of the Moog VCF in the digital domain",
;;; ICMC07, Copenhagen.
(define-vug moogff (in freq resonance (reset-p boolean))
  (with-samples (y s s1 s2 s3 s4 u past future)
    (with ((wc (* 2.0d0 (tan (* pi freq *sample-duration*))
                  *sample-rate*))
           (wct (if (plusp wc) (* *sample-duration* wc) 0.0d0))
           (b0 (/ wct (+ wct 2.0d0)))
           (b0b0 (* b0 b0))
           (b0b0b0b0 (* b0b0 b0b0))
           (a1 (/ (- wct 2.0d0) (+ wct 2.0d0)))
           (reset (when reset-p
                    (setf s1 0.0d0 s2 0.0d0 s3 0.0d0 s4 0.0d0
                          reset-p nil))))
      (declare (type sample wc wct b0 b0b0 b0b0b0b0 a1))
      ;; No warning
      (initialize reset)
      (setf s (+ s4 (* b0 (+ s3 (* b0 (+ s2 (* b0 s1)))))))
      (setf y (* (+ (* b0b0b0b0 in) s)
                 (/ 1.0d0 (+ 1.0d0 (* b0b0b0b0 resonance)))))
      (setf u (- in (* resonance y)))
      (setf past u)
      (setf future (+ (* b0 past) s1))
      (setf s1 (- (* b0 past) (* a1 future)))
      (setf past future)
      (setf future (+ (* b0 past) s2))
      (setf s2 (- (* b0 past) (* a1 future)))
      (setf past future)
      (setf future (+ (* b0 past) s3))
      (setf s3 (- (* b0 past) (* a1 future)))
      (setf s4 (- (* b0 future) (* a1 y)))
      y)))
