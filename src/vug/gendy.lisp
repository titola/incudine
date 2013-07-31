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

;;; Implementation of the G�n�ration Dynamique Stochastique (GENDYN),
;;; a dynamic stochastic approach to waveform synthesis conceived by
;;; Iannis Xenakis. See Formalized Music (1992, Stuyvesant, NY:
;;; Pendragon Press), pp. 246 - 254, 289 - 322.
;;; Based on Nick Collins's Gendy1 ugen (SuperCollider)

;; (define-vug gendy-distribution ((which fixnum) a)
;;   (with-samples ((a (clip a 0.0001d0 1.0d0))
;;                  (c 0.0d0)
;;                  (r 0.0d0))
;;     (cond ((= which 1) ; cauchy
;;            (setf c (atan (* 10.0d0 a)))
;;            (* 0.1d0 (/ 1.0d0 a) (tan (* c (- (* 2.0d0 (random 1.0d0)) 1.0d0)))))
;;           ((= which 2) ; logist
;;            (setf c (+ .5d0 (* .499d0 a)))
;;            (setf c (the sample (log (/ (- 1.0d0 c) c))))
;;            (setf r (+ (* (- (random 0.998d0) 0.499d0) a) 0.5d0))
;;            (/ (the sample (log (/ (- 1.0d0 r) r))) c))
;;           ((= which 3) ; hyperbcos
;;            (setf c (tan (* 1.5692255d0 a)))
;;            (setf r (/ (tan (* 1.5692255d0 a (random 1.0d0))) c))
;;            (setf r (* (the sample (log (+ (* r 0.999d0) .001d0)))
;;                       -0.1447648d0))
;;            (- (* 2.0d0 r) 1.0d0))
;;           ((= which 4) ; arcsine
;;            (setf c (sin (* 1.5707963d0 a)))
;;            (/ (sin (* pi (- (random 1.0d0) 0.5d0) a)) c))
;;           ((= which 5) ; expon
;;            (setf c (the sample (log (- 1.0d0 (* 0.999d0 a)))))
;;            (setf r (/ (the sample (log (- 1.0d0 (* (random 0.999d0) a)))) c))
;;            (- (* 2.0d0 r) 1.0d0))
;;           ((= which 6) ; external distribution
;;            (- (* a 2.0d0) 1.0d0))
;;           ;; linear
;;           (t (- (random 2.0d0) 1d0)))))


;;; The distributions are not the originals but rewritten by
;;; Nick Collins for SuperCollider to fix some numerical problems
(declaim (inline gendy-cauchy))
(defun gendy-cauchy (a)
  (* 0.1d0 (/ 1.0d0 a) (tan (* (atan (* 10.0d0 a))
                               (- (* 2.0d0 (random 1.0d0)) 1.0d0)))))

(declaim (inline gendy-logist))
(defun gendy-logist (a)
  (let* ((b (+ .5d0 (* .499d0 a)))
         (c (the sample (log (/ (- 1.0d0 b) b))))
         (r (+ (* (- (random 0.998d0) 0.499d0) a) 0.5d0)))
    (/ (the sample (log (/ (- 1.0d0 r) r))) c)))

(declaim (inline gendy-hyperbcos))
(defun gendy-hyperbcos (a)
  (let ((r (/ (tan (the maybe-limited-sample
                     (* 1.5692255d0 a (random 1.0d0))))
              (tan (the maybe-limited-sample
                     (* 1.5692255d0 a))))))
    (- (* 2.0d0 (* (the sample (log (+ (* r 0.999d0) .001d0))) -0.1447648d0))
       1.0d0)))

(declaim (inline gendy-arcsine))
(defun gendy-arcsine (a)
  (/ (sin (the maybe-limited-sample (* pi (- (random 1.0d0) 0.5d0) a)))
     (sin (the maybe-limited-sample (* 1.5707963d0 a)))))

(declaim (inline gendy-expon))
(defun gendy-expon (a)
  (- (* 2.0d0 (/ (the sample (log (- 1.0d0 (* (random 0.999d0) a))))
                 (the sample (log (- 1.0d0 (* 0.999d0 a))))))
     1.0d0))

(declaim (inline gendy-distribution))
(defun gendy-distribution (which a)
  (declare #.*reduce-warnings*)
  (let ((a (clip a 0.0001d0 1.0d0)))
    (cond ((= which 1) (gendy-cauchy a))
          ((= which 2) (gendy-logist a))
          ((= which 3) (gendy-hyperbcos a))
          ((= which 4) (gendy-arcsine a))
          ((= which 5) (gendy-expon a))
          ((= which 6) (- (* a 2.0d0) 1.0d0))  ; external
          (t (- (random 2.0d0) 1d0)))))        ; linear

(defmacro gendy-update-value (data index dist-var dist-value
                              dist-param scale min max)
  `(progn
     (setf ,dist-var (gendy-distribution ,dist-value ,dist-param))
     (setf (data-ref ,data ,index)
           (mirror (+ (data-ref ,data ,index) (* ,scale ,dist-var))
                   ,min ,max))))

;;; The waveform is generated by USED-POINTS - 1 segments and it is
;;; repeated in the time. The vertexes (control points) are moved
;;; according to a stochastic action and they are limited within the
;;; boundaries of a mirror formed by an amplitude barrier and a time
;;; barrier.
(define-vug-macro gendy (amp-distr dur-distr amp-distr-param
                         dur-distr-param freq-min freq-max amp-scale
                         dur-scale &optional (max-points 12)
                         (used-points max-points) (interpolation :linear))
  (if (constantp max-points)
      (with-gensyms (amps-wrap durs-wrap amps durs distrib index i
                     adist ddist points freq-delta)
        (with-coerce-arguments (amp-distr-param dur-distr-param freq-min
                                freq-max amp-scale dur-scale)
          `(with ((,amps-wrap (make-foreign-array ,max-points 'sample))
                  (,durs-wrap (make-foreign-array ,max-points 'sample))
                  (,amps (foreign-array-data ,amps-wrap))
                  (,durs (foreign-array-data ,durs-wrap))
                  (,adist ,amp-distr)
                  (,ddist ,dur-distr)
                  (,distrib 0.0d0)
                  (,freq-delta (- ,freq-max ,freq-min))
                  (,index 0)
                  (,points (clip (the fixnum ,used-points) 1 ,max-points)))
             (declare (type sample ,distrib ,freq-delta)
                      (type non-negative-fixnum ,index ,adist ,ddist ,points))
             (initialize
              (dotimes (,i ,max-points)
                (setf (data-ref ,amps ,i) (- (random 2.0d0) 1.0d0)
                      (data-ref ,durs ,i) (random 1.0d0))))
             (interpolate
              (tick
               (prog1 (data-ref ,amps ,index)
                 (when (>= (incf ,index) ,points)
                   (setf ,index 0))
                 (gendy-update-value ,amps ,index ,distrib ,adist
                                     ,amp-distr-param ,amp-scale -1.0d0 1.0d0)
                 (gendy-update-value ,durs ,index ,distrib ,ddist
                                     ,dur-distr-param ,dur-scale 0.0d0 1.0d0)))
              (* (+ ,freq-min (* ,freq-delta (data-ref ,durs ,index))) ,points)
              ,interpolation))))
      (error "MAX-POINTS of GENDY is not a constant")))