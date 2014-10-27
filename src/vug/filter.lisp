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

(define-vug delay1 (in)
  (with-samples (sig) (prog1 sig (setf sig in))))

(define-vug one-pole (in coef)
  (with-samples (y1)
    (setf y1 (+ in (* coef (- y1 in))))))

(define-vug one-zero (in coef)
  (with-samples ((x1 0.0d0)
                 (x (prog1 (if (minusp coef) (+ x1 in) (- x1 in))
                      (setf x1 in))))
    (+ in (* coef x))))

(define-vug two-pole (in freq radius)
  (with-samples ((a1 (* 2 radius (cos (* freq *twopi-div-sr*))))
                 (a2 (- (* radius radius)))
                 (y0 0.0d0)
                 (y1 0.0d0)
                 (y2 0.0d0))
    (prog1 (setf y0 (+ in (* a1 y1) (* a2 y2)))
      (setf y2 y1 y1 y0))))

(define-vug two-zero (in freq radius)
  (with-samples ((b1 (* -2 radius (cos (* freq *twopi-div-sr*))))
                 (b2 (* radius radius))
                 (x1 0.0d0)
                 (x2 0.0d0))
    (prog1 (+ in (* b1 x1) (* b2 x2))
      (setf x2 x1 x1 in))))

(define-vug dcblock (in coef)
  (with-samples (x1 y1)
    (prog1 (setf y1 (+ (- in x1) (* coef y1)))
      (setf x1 in))))

;;; One pole filter with the coefficient calculated from a 60 dB lag time
(define-vug lag (in time)
  (with-samples ((coef (t60->pole time)))
    (one-pole in coef)))

(define-vug lag-ud (in attack-time decay-time)
  (with-samples ((y1 0.0d0)
                 (coef-up (t60->pole attack-time))
                 (coef-down (t60->pole decay-time)))
    (setf y1 (+ in (* (if (> in y1) coef-up coef-down)
                      (- y1 in))))))

(define-vug env-follower (in attack-time decay-time)
  (lag-ud (abs in) attack-time decay-time))

(define-vug decay (in decay-time)
  (with-samples ((y1 0.0d0)
                 (b1 (t60->pole decay-time)))
    ;; Update the input, if it is required, to avoid the expansion
    ;; inside the next condition
    (maybe-expand in)
    (setf y1 (if (zerop b1) in (+ in (* b1 y1))))))

(define-vug decay-2 (in attack-time decay-time)
  (- (decay in decay-time) (decay in attack-time)))

(define-vug biquad (in b0 b1 b2 a0 a1 a2)
  (with-samples (x1 x2 y y1 y2 (a0r (/ a0)))
    (setf y (- (+ (* b0 a0r in)
                  (* b1 a0r x1)
                  (* b2 a0r x2))
               (* a1 a0r y1)
               (* a2 a0r y2))
          x2 x1 x1 in y2 y1 y1 y)
    y))

;;; Two pole resonant filters.
;;;
;;; References:
;;;
;;;   [1] Julius O. Smith and James B. Angell in "A Constant Gain
;;;   Digital Resonator Tuned by a Single Coefficient," Computer Music
;;;   Journal, Vol. 6, No. 4, Winter 1982, p.36-39.
;;;
;;;   [2] Ken Steiglitz, "A Note on Constant-Gain Digital Resonators,"
;;;   Computer Music Journal, vol. 18, no. 4, pp. 8-10, Winter 1982.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-reson-common ((wt-var radius-var freq q) &body body)
    `(with-samples ((,wt-var (* ,freq *twopi-div-sr*))
                    (,radius-var (exp (* (/ ,freq ,q)
                                         *minus-pi-div-sr*))))
       ,@body)))

(define-vug reson (in freq q)
  "Two pole resonant filter."
  (with-reson-common (wt r freq q)
    (with-samples ((rr (* r r)))
      (biquad in (* (- 1 rr) (sin wt)) 0 0 1 (- (* 2 r (cos wt))) rr))))

(define-vug resonz (in freq q)
  "Two pole resonant filter with zeroes located at z = 1 and z = -1."
  (with-reson-common (wt r freq q)
    (with-samples ((rr (* r r))
                   (scale (* (- 1 rr) 0.5)))
      (biquad in scale 0 (- scale) 1 (- (* 2 r (cos wt))) rr))))

(define-vug resonr (in freq q)
  "Two pole resonant filter with zeroes located at +/- sqrt(R)."
  (with-reson-common (wt r freq q)
    (with-samples ((scale (- 1 r)))
      (biquad in scale 0 (* scale (- r)) 1 (- (* 2 r (cos wt))) (* r r)))))

;;; It is the same as RESONZ with the bandwidth specified in a 60dB
;;; ring decay time and a constant scale factor.
;;; Inspired by Ringz in SuperCollider.
(define-vug ringz (in freq decay-time)
  (with-samples ((wt (* freq *twopi-div-sr*))
                 (r (t60->pole decay-time))
                 (rr (* r r))
                 (scale 0.5))
    (biquad in scale 0 (- scale) 1 (- (* 2 r (cos wt))) rr)))

;;; It is the same as RESONR with the bandwidth specified in a 60dB
;;; ring decay time and a constant scale factor.
(define-vug ringr (in freq decay-time)
  (with-samples ((wt (* freq *twopi-div-sr*))
                 (r (t60->pole decay-time))
                 (scale 0.5))
    (biquad in scale 0 (* scale (- r)) 1 (- (* 2 r (cos wt))) (* r r))))

;;; FOF-like filter based on James McCartney's Formlet.
;;; The name is FOFILTER (used also in Csound) to avoid confusion with
;;; FORMLET and LET.
(define-vug fofilter (in freq attack-time decay-time)
  (with-samples ((wt (* freq *twopi-div-sr*))
                 (cos-wt (cos wt))
                 (r0 (t60->pole attack-time))
                 (r1 (t60->pole decay-time)))
    (- (biquad in 0.25 0 -0.25 1 (- (* 2 r1 cos-wt)) (* r1 r1))
       (biquad in 0.25 0 -0.25 1 (- (* 2 r0 cos-wt)) (* r0 r0)))))

;;; EQ biquad filter coefficients by Robert Bristow-Johnson
;;; http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %%with-biquad-common (bindings &body body)
    `(with-samples ((w0 (* freq *twopi-div-sr*))
                    (cos-w0 (cos w0))
                    (sin-w0 (sin w0))
                    ,@bindings)
       ,@body))

  (defmacro %with-biquad-common (bindings &body body)
    `(%%with-biquad-common
         ((alpha (* sin-w0 (if (plusp q) (/ 0.5 q) 1000.0)))
          ,@bindings)
       ,@body))

  (defmacro %with-biquad-shelf-common (&body body)
    `(%%with-biquad-common
         ((gain (expt (sample 10) (* db (sample 0.025))))
          (alpha (* sin-w0 0.5
                    (sqrt (the non-negative-sample
                            (+ 2.0 (* (+ gain (/ gain))
                                      (- (/ s) 1.0)))))))
          (c1 (+ gain 1.0))
          (c2 (- gain 1.0))
          (c3 (* c1 cos-w0))
          (c4 (* c2 cos-w0))
          (c5 (* 2 (sqrt (the non-negative-sample gain)) alpha)))
       ,@body)))

(define-vug lpf (in freq q)
  (%with-biquad-common
      ((b1 (- 1.0 cos-w0))
       (b2 (* b1 0.5)))
    (biquad in b2 b1 b2 (+ 1.0 alpha) (- (* 2.0 cos-w0)) (- 1.0 alpha))))

(define-vug hpf (in freq q)
  (%with-biquad-common
      ((b1 (- (+ 1.0 cos-w0)))
       (b2 (* (- b1) 0.5)))
    (biquad in b2 b1 b2 (+ 1.0 alpha) (- (* 2.0 cos-w0)) (- 1.0 alpha))))

(define-vug bpf (in freq q)
  (%with-biquad-common ()
    (biquad in alpha 0.0 (- alpha) (+ 1.0 alpha) (- (* 2.0 cos-w0))
            (- 1.0 alpha))))

(define-vug notch (in freq q)
  (%with-biquad-common
      ((b1 (- (* 2.0 cos-w0))))
    (biquad in 1.0 b1 1.0 (+ 1.0 alpha) b1 (- 1.0 alpha))))

(define-vug apf (in freq q)
  (%with-biquad-common
      ((b0 (- 1.0 alpha))
       (b1 (- (* 2.0 cos-w0)))
       (b2 (+ 1.0 alpha)))
    (biquad in b0 b1 b2 b2 b1 b0)))

(define-vug peak-eq (in freq q db)
  (%with-biquad-common
      ((gain (expt (sample 10) (* db (sample 0.025))))
       (c1 (* alpha gain))
       (c2 (/ alpha gain))
       (b1 (- (* 2.0 cos-w0))))
    (biquad in (+ 1.0 c1) b1 (- 1.0 c1) (+ 1.0 c2) b1 (- 1.0 c2))))

(define-vug low-shelf (in freq s db)
  (%with-biquad-shelf-common
    (biquad in (* gain (+ (- c1 c4) c5))
            (* 2 gain (- c2 c3))
            (* gain (- c1 c4 c5))
            (+ c1 c4 c5)
            (* -2.0 (+ c2 c3))
            (- (+ c1 c4) c5))))

(define-vug hi-shelf (in freq s db)
  (%with-biquad-shelf-common
    (biquad in (* gain (+ (- c1 c4) c5))
            (* -2 gain (+ c2 c3))
            (* gain (- (+ c1 c4) c5))
            (+ (- c1 c4) c5)
            (* 2.0 (- c2 c3))
            (- c1 c4 c5))))

;;; Second order Butterworth filters.
;;; Formulas from Charles Dodge, "Computer music: synthesis,
;;; composition, and performance"
(defmacro %butter-filter (in c1 c2 c3 c4 c5)
  (with-gensyms (value old1 old2)
    `(with-samples (,old1 ,old2)
       (let ((,value (- ,in (* ,c4 ,old1) (* ,c5 ,old2))))
         (prog1 (+ (* ,value ,c1) (* ,c2 ,old1) (* ,c3 ,old2))
           (setf ,old2 ,old1 ,old1 ,value))))))

(define-vug butter-lp (in fcut)
  (with-samples ((c (/ 1.0 (tan (* fcut *pi-div-sr*))))
                 (cc (* c c))
                 (sqrt2-mult-c (* +sqrt2+ c))
                 (c1 (/ 1.0 (+ 1.0 sqrt2-mult-c cc)))
                 (c2 (+ c1 c1))
                 (c4 (* 2.0 (- 1.0 cc) c1))
                 (c5 (* (+ (- 1.0 sqrt2-mult-c) cc) c1)))
    (%butter-filter in c1 c2 c1 c4 c5)))

(define-vug butter-hp (in fcut)
  (with-samples ((c (tan (* fcut *pi-div-sr*)))
                 (cc (* c c))
                 (sqrt2-mult-c (* +sqrt2+ c))
                 (c1 (/ 1.0 (+ 1.0 sqrt2-mult-c cc)))
                 (c2 (- (+ c1 c1)))
                 (c4 (* 2.0 (- cc 1.0) c1))
                 (c5 (* (+ (- 1.0 sqrt2-mult-c) cc) c1)))
    (%butter-filter in c1 c2 c1 c4 c5)))

(define-vug butter-bp (in fcut bandwidth)
  (with-samples ((c (/ 1.0 (tan (* bandwidth *pi-div-sr*))))
                 (d (* 2.0 (cos (* fcut *twopi-div-sr*))))
                 (c1 (/ 1.0 (+ 1.0 c)))
                 (c3 (- c1))
                 (c4 (* (- c) d c1))
                 (c5 (* (- c 1.0) c1)))
    (%butter-filter in c1 0.0 c3 c4 c5)))

(define-vug butter-br (in fcut bandwidth)
  (with-samples ((c (tan (* bandwidth *pi-div-sr*)))
                 (d (* 2.0 (cos (* fcut *twopi-div-sr*))))
                 (c1 (/ 1.0 (+ 1.0 c)))
                 (c2 (- (* d c1)))
                 (c5 (* (- 1.0 c) c1)))
    (%butter-filter in c1 c2 c1 c2 c5)))

;;; Digital emulation of a 3 pole lowpass filter. Based on Josep
;;; Comajuncosas' 18dB/oct resonant 3-pole LPF with tanh dist (Csound)
(define-vug lpf18 (in freq resonance distortion)
  (with-samples ((f (* 2.0 freq *sample-duration*))
                 (p (- (* (+ (* (+ (* -2.7528 f) 3.0429) f) 1.718) f) 0.9984))
                 (p1 (+ p 1.0))
                 (p1h (* p1 0.5))
                 (res-scale (+ (* (- (* (+ (* -2.7079 p1) 10.963) p1) 14.934) p1)
                               8.4974))
                 ;; Alternative multiplier for the resonance
                 ;; (res-scale (- 2.2173 (* 1.6519 (the sample (log p1)))))
                 (res (* resonance res-scale))
                 (value (+ 1.0 (* distortion (+ 1.5 (* 2.0 res (- 1.0 f))))))
                 ;; Initialized with zero
                 x0 x1 y1 y11 y2 y31 out)
      (setf x0  (- in (tanh (* res out)))
            y1  (- (* p1h (+ x0 x1))  (* p y1))
            y2  (- (* p1h (+ y1 y11)) (* p y2))
            out (- (* p1h (+ y2 y31)) (* p out)))
      (setf x1 x0 y11 y1 y31 y2)
      (tanh (* out value))))

;;; State Variable Filter.
;;; Reference: http://www.musicdsp.org/archive.php?classid=3#92
;;;
;;; This filter has five simultaneous outputs stored in a FRAME:
;;; low-pass, high-pass, band-pass, notch and peaking.
;;;
;;; It is stable with RESONANCE from 0 to 1 and DRIVE from 0 to 0.1
(define-vug svf (in fcut resonance drive)
  (with ((freq (* 2.0 (sin (* pi (min 0.25 (* fcut
                                              ;; Double sampled
                                              0.5 *sample-duration*))))))
         ;; Inferior limit for the resonance, however it is safer a
         ;; value not greater than one.
         (res (if (minusp resonance) +sample-zero+ resonance))
         (damp (min (* 2.0 (- 1.0 (expt (the non-negative-sample res) 0.25)))
                    (min 2.0 (- (/ 2.0 freq) (* freq 0.5)))))
         (frame (make-frame 5 :zero-p t)))
    (declare (type sample freq res damp))
    (with-samples (low high band notch)
      (multiple-sample-bind (lp hp bp rb peak) frame
        ;; First pass
        (setf notch (- in (* damp band))
              low   (+ low (* freq band))
              high  (- notch low)
              band  (- (+ (* freq high) band) (* drive band band band)))
        (setf lp (* 0.5 low)
              hp (* 0.5 high)
              bp (* 0.5 band)
              rb (* 0.5 notch))
        ;; Second pass
        (setf notch (- in (* damp band))
              low   (+ low (* freq band))
              high  (- notch low)
              band  (- (+ (* freq high) band) (* drive band band band)))
        (setf lp (+ lp (* 0.5 low))
              hp (+ hp (* 0.5 high))
              bp (+ bp (* 0.5 band))
              rb (+ rb (* 0.5 notch))
              peak (- lp hp)))
      frame)))

;;; Moving Average Filter
(define-vug maf (in (max-size positive-fixnum) (size positive-fixnum))
  (with ((array-wrap (make-foreign-array max-size 'sample :zero-p t))
         (data (foreign-array-data array-wrap))
         (sum 0.0d0)
         (old-size 0)
         (size (prog1 (min size max-size)
                 (when (< 0 size old-size)
                   ;; Update the sum
                   (loop for i from size below old-size do
                        (decf sum (smp-ref data i))
                        (setf (smp-ref data i) +sample-zero+)))
                 (setf old-size size)))
         (index 0))
    (declare (type foreign-array array-wrap)
             (type foreign-pointer data) (type sample sum)
             (type non-negative-fixnum index old-size)
             (type positive-fixnum size))
    ;; Subtract the old, add the new and update the index
    (setf sum (+ (- sum (smp-ref data index)) in))
    (setf (smp-ref data index) in)
    (let ((new (1+ index)))
      (setf index (if (>= index size) 0 new)))
    (/ sum size)))

;;; Median filter based on James McCartney's Median ugen (SuperCollider).
;;; Added the code to modulate the window size of the filter.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline %median-update-value))
  (defun %median-update-value (values ages src-pos dest-pos)
    (setf (smp-ref values dest-pos) (smp-ref values src-pos)
          (svref ages dest-pos) (svref ages src-pos)))

  (declaim (inline %median-shrink))
  (defun %median-shrink (values ages old-size new-size)
    (declare (type foreign-pointer values) (type simple-vector ages)
             (type positive-fixnum old-size new-size))
    (let ((pos 0))
      (declare (type non-negative-fixnum pos))
      (dotimes (i old-size)
        (when (< (svref ages i) new-size)
          (unless (= pos i)
            (%median-update-value values ages i pos))
          (incf pos)))))

  (declaim (inline %median-expand))
  (defun %median-expand (values ages old-size new-size)
    (declare (type foreign-pointer values) (type simple-vector ages)
             (type positive-fixnum old-size new-size))
    (let ((right-shift (ash (+ (- new-size old-size) 1) -1))
          (pos (1- new-size))
          (old-last (1- old-size))
          (age old-size))
      (dotimes (i right-shift)
        (setf (smp-ref values pos) (smp-ref values old-last))
        (setf (svref ages pos) age)
        (decf pos)
        (incf age))
      (dotimes (i old-size)
        (setf (smp-ref values pos) (smp-ref values old-last))
        (decf pos)
        (decf old-last))
      (do ((old-first (1+ pos))
           (i 0 (1+ i)))
          ((> i pos))
        (setf (smp-ref values i) (smp-ref values old-first))
        (setf (svref ages i) age)
        (incf age))))

  (defmacro %median-update-ages (position-var ages size last)
    (with-gensyms (index)
      `(dotimes (,index ,size)
         (if (= (the fixnum (svref ,ages ,index)) ,last)
             (setf ,position-var ,index)  ; position of the oldest value
             (incf (the fixnum (svref ,ages ,index)))))))

  (defmacro %median-search-lower (input values ages pos)
    (with-gensyms (prev-pos)
      `(progn
         ,input
         (loop for ,prev-pos = (the fixnum (1- ,pos))
               while (and (plusp ,pos)
                          (< ,input (smp-ref ,values ,prev-pos))) do
              (%median-update-value ,values ,ages ,prev-pos ,pos)
              (decf ,pos)))))

  (defmacro %median-search-higher (input values ages pos last)
    (with-gensyms (next-pos)
      `(loop for ,next-pos = (the non-negative-fixnum (1+ ,pos))
             while (and (/= ,pos ,last)
                        (> ,input (smp-ref ,values ,next-pos))) do
            (%median-update-value ,values ,ages ,next-pos ,pos)
            (incf ,pos)))))

(define-vug median (in (max-size positive-fixnum) (size positive-fixnum))
  "Median filter."
  (with ((values-wrap (without-follow (max-size)
                        (make-foreign-array max-size 'sample :zero-p t)))
         (values (foreign-array-data values-wrap))
         (ages (without-follow (max-size) (make-array max-size)))
         (old-size 0)
         (init-pass-p t)
         (size (without-follow (max-size)
                 (let ((new-size (min size max-size)))
                   (cond (init-pass-p (setf init-pass-p nil))
                         ((< new-size old-size)
                          (%median-shrink values ages old-size new-size))
                         ((> new-size old-size)
                          (%median-expand values ages old-size new-size)))
                   (setf old-size new-size)
                   new-size)))
         (last (1- size))
         (median (ash size -1))
         (pos 0))
    (declare (type non-negative-fixnum pos old-size size last median)
             (type boolean init-pass-p) (type foreign-array values-wrap)
             (type foreign-pointer values) (type simple-vector ages))
    (initialize
     ;; Expand the input, if it is required, to avoid the expansion
     ;; inside the next loop
     (maybe-expand in)
     (dotimes (i size)
       ;(setf (smp-ref values i) in)
       (setf (svref ages i) i)))
    (%median-update-ages pos ages size last)
    (%median-search-lower in values ages pos)
    (%median-search-higher in values ages pos last)
    (setf (smp-ref values pos) in)
    (setf (svref ages pos) 0)
    (smp-ref values median)))

;;; Second order normalized digital waveguide resonator.
;;;
;;; Reference:
;;;
;;;   [1] Julius O. Smith, "Faust Filter Library" (filter.lib)
;;;
;;;   [2] https://ccrma.stanford.edu/~jos/pasp/Power_Normalized_Waveguide_Filters.html
;;;
(define-vug nlf2 (in freq r)
  "Second order normalized digital waveguide resonator."
  (with-samples ((th (* +twopi+ freq *sample-duration*))
                 (c (cos th))
                 (s (sin th))
                 (fb0 0)
                 (fb1 0))
    (with ((frame (make-frame 2)))
      (multiple-sample-bind (sin-out cos-out) frame
        (setf sin-out (* r (+ (* fb1 s) (* fb0 c)))
              cos-out (* (+ in (+ (* fb1 c) (* fb0 (- s))))))
        (setf fb0 sin-out fb1 cos-out)
        frame))))
