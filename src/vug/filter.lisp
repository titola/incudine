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

;;; Note: nested anaphoric VUG-MACROs are safe, because the
;;; VUG-VARIABLE IT becomes a new symbol created with GENSYM.
;;;
;;; The symbol ~ is inspired by FAUST programming language.
(define-vug-macro ~ (in &key (type 'sample) (initial-value 0))
  "Anaphoric VUG MACRO for recursive composition."
  (let ((it (ensure-symbol 'it)))
    `(with ((,it ,(coerce-number initial-value type)))
       (declare (type ,type ,it))
       (setf ,it ,in))))

(define-vug delay1 (in)
  "One sample delay."
  (with-samples (sig) (prog1 sig (setf sig in))))

(define-vug pole (in coef)
  "One pole filter."
  (~ (+ in (* coef it))))

(define-vug pole* (in coef)
  "Scaled one pole filter."
  (with-samples ((g (- 1 (abs coef))))
    (pole (* g in) coef)))

(define-vug integrator (in)
  "Integrate the input IN."
  (pole in 1))

(define-vug zero (in coef)
  "One zero filter."
  (- in (* coef (delay1 in))))

(define-vug zero* (in coef)
  "Scaled one zero filter."
  (with-samples ((g (/ (1+ (abs coef)))))
    (* g (zero in coef))))

(define-vug diff (in)
  "First order difference."
  (zero in 1))

(define-vug lag (in time)
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME."
  (pole* in (t60->pole time)))

(define-vug lag-ud (in attack-time decay-time)
"Scaled one pole filter with the coefficient calculated from
a 60 dB lag ATTACK-TIME and DECAY-TIME."
(with-samples ((coef-up (t60->pole attack-time))
               (coef-down (t60->pole decay-time)))
  (~ (pole* in (if (> in it) coef-up coef-down)))))

(define-vug biquad (in b0 b1 b2 a0 a1 a2)
  "Biquad filter."
  (with-samples (x1 x2 y y1 y2)
    (with-samples ((a0r (/ a0))
                   (n0 (* b0 a0r))
                   (n1 (* b1 a0r))
                   (n2 (* b2 a0r))
                   (d1 (* a1 a0r))
                   (d2 (* a2 a0r)))
      (setf y (- (+ (* n0 in) (* n1 x1) (* n2 x2)) (* d1 y1) (* d2 y2))
            x2 x1 x1 in y2 y1 y1 y)
      y)))

(defmacro with-reson-common ((wt-var radius-var freq q) &body body)
  `(with-samples ((,wt-var (hz->radians ,freq))
                  (,radius-var (exp (* (/ ,freq ,q) *minus-pi-div-sr*))))
     ,@body))

;;; `tone' and `atone' opcodes used in Csound.
(define-vug cs-tone (in hp)
  "A first-order recursive lowpass filter where HP is the response curve's
half-power point."
  (with-samples ((b (- 2 (cos (hz->radians hp))))
                 (coef (- b (sqrt (the non-negative-sample (- (* b b) 1)))))
                 (g (- 1 coef)))
    (pole (* g in) coef)))

(define-vug cs-atone (in hp)
  "A first-order recursive highpass filter where HP is the response curve's
half-power point."
  (with-samples ((b (- 2 (cos (hz->radians hp))))
                 (coef (- b (sqrt (the non-negative-sample (- (* b b) 1))))))
    (pole (* coef (zero in 1)) coef)))

(define-vug two-pole (in freq radius)
  "Two pole filter."
  (with-samples ((a1 (* -2 radius (cos (hz->radians freq))))
                 (a2 (* radius radius)))
    (~ (- in (* a1 it) (* a2 (delay1 it))))))

(define-vug two-zero (in freq radius)
  "Two zero filter."
  (with-samples ((b1 (* -2 radius (cos (hz->radians freq))))
                 (b2 (* radius radius))
                 (x1 (delay1 in)))
    (+ in (* b1 x1) (* b2 (delay1 x1)))))

(define-vug dcblock (in coef)
  "DC blocking filter."
  (pole (zero in 1) coef))

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
  "Two pole resonant filter with zeroes located at +/- sqrt(radius)."
  (with-reson-common (wt r freq q)
    (with-samples ((scale (- 1 r)))
      (biquad in scale 0 (* scale (- r)) 1 (- (* 2 r (cos wt))) (* r r)))))

;;; Inspired by Ringz in SuperCollider.
(define-vug ringz (in freq decay-time)
  "Two pole resonant filter with zeroes located at z = 1 and z = -1.
The bandwidth is specified in a 60dB ring DECAY-TIME and a constant
scale factor."
  (with-samples ((wt (hz->radians freq))
                 (r (t60->pole decay-time))
                 (rr (* r r))
                 (scale 0.5))
    (biquad in scale 0 (- scale) 1 (- (* 2 r (cos wt))) rr)))

(define-vug ringr (in freq decay-time)
  "Two pole resonant filter with zeroes located at +/- sqrt(radius).
The bandwidth is specified in a 60dB ring DECAY-TIME and a constant
scale factor."
  (with-samples ((wt (hz->radians freq))
                 (r (t60->pole decay-time))
                 (scale 0.5))
    (biquad in scale 0 (* scale (- r)) 1 (- (* 2 r (cos wt))) (* r r))))

;;; FOF-like filter based on James McCartney's Formlet.
;;; The name is FOFILTER (used also in Csound) to avoid confusion with
;;; FORMLET and LET.
(define-vug fofilter (in freq attack-time decay-time)
  "FOF-like filter."
  (with-samples ((wt (hz->radians freq))
                 (cos-wt (cos wt))
                 (r0 (t60->pole attack-time))
                 (r1 (t60->pole decay-time)))
    (- (biquad in 0.25 0 -0.25 1 (- (* 2 r1 cos-wt)) (* r1 r1))
       (biquad in 0.25 0 -0.25 1 (- (* 2 r0 cos-wt)) (* r0 r0)))))

;;; EQ biquad filter coefficients by Robert Bristow-Johnson
;;; http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

(defmacro %%with-biquad-common (bindings &body body)
  `(with-samples ((w0 (hz->radians freq))
                  (cos-w0 (cos w0))
                  (sin-w0 (sin w0))
                  ,@bindings)
     ,@body))

(defmacro %with-biquad-common (bindings &body body)
  `(%%with-biquad-common
       ((alpha (* sin-w0 (if (plusp q) (/ 0.5 q) (sample 1000))))
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
     ,@body))

(define-vug lpf (in freq q)
  "Second-order lowpass filter."
  (%with-biquad-common
      ((b1 (- 1.0 cos-w0))
       (b2 (* b1 0.5)))
    (biquad in b2 b1 b2 (+ 1.0 alpha) (- (* 2.0 cos-w0)) (- 1.0 alpha))))

(define-vug hpf (in freq q)
  "Second-order highpass filter."
  (%with-biquad-common
      ((b1 (- (+ 1.0 cos-w0)))
       (b2 (* (- b1) 0.5)))
    (biquad in b2 b1 b2 (+ 1.0 alpha) (- (* 2.0 cos-w0)) (- 1.0 alpha))))

(define-vug bpf (in freq q)
  "Second-order bandpass filter."
  (%with-biquad-common ()
    (biquad in alpha 0.0 (- alpha) (+ 1.0 alpha) (- (* 2.0 cos-w0))
            (- 1.0 alpha))))

(define-vug notch (in freq q)
  "Second-order notch filter."
  (%with-biquad-common
      ((b1 (- (* 2.0 cos-w0))))
    (biquad in 1.0 b1 1.0 (+ 1.0 alpha) b1 (- 1.0 alpha))))

(define-vug apf (in freq q)
  "Second-order allpass filter."
  (%with-biquad-common
      ((b0 (- 1.0 alpha))
       (b1 (* -2.0 cos-w0))
       (b2 (+ 1.0 alpha)))
    (biquad in b0 b1 b2 b2 b1 b0)))

(define-vug peak-eq (in freq q db)
  "Second-order peaking equalizer."
  (%with-biquad-common
      ((gain (expt (sample 10) (* db (sample 0.025))))
       (c1 (* alpha gain))
       (c2 (/ alpha gain))
       (b1 (- (* 2.0 cos-w0))))
    (biquad in (+ 1.0 c1) b1 (- 1.0 c1) (+ 1.0 c2) b1 (- 1.0 c2))))

(define-vug low-shelf (in freq s db)
  "Second-order low shelf filter."
  (%with-biquad-shelf-common
    (biquad in (* gain (+ (- c1 c4) c5))
            (* 2 gain (- c2 c3))
            (* gain (- c1 c4 c5))
            (+ c1 c4 c5)
            (* -2.0 (+ c2 c3))
            (- (+ c1 c4) c5))))

(define-vug hi-shelf (in freq s db)
  "Second-order high shelf filter."
  (%with-biquad-shelf-common
    (biquad in (* gain (+ (- c1 c4) c5))
            (* -2 gain (+ c2 c3))
            (* gain (- (+ c1 c4) c5))
            (+ (- c1 c4) c5)
            (* 2.0 (- c2 c3))
            (- c1 c4 c5))))

;;; Second order Butterworth filters obtained from the analog prototypes after
;;; the application of the bilinear transform: s = c * (1 - z^-1) / (1 + z^-1)
(define-vug butter-lp (in fcut)
  "Second-order Butterworth lowpass filter."
  (with-samples ((c (/ 1.0 (tan (* fcut *pi-div-sr*))))
                 (cc (* c c))
                 (sqrt2-mult-c (* +sqrt2+ c)))
    (biquad in 1 2 1 (+ cc sqrt2-mult-c 1) (* 2 (- 1 cc))
            (- (+ cc 1) sqrt2-mult-c))))

(define-vug butter-hp (in fcut)
  "Second-order Butterworth highpass filter."
  (with-samples ((c (tan (* fcut *pi-div-sr*)))
                 (cc (* c c))
                 (sqrt2-mult-c (* +sqrt2+ c)))
    (biquad in 1 -2 1 (+ cc sqrt2-mult-c 1) (* 2 (- cc 1))
            (- (+ cc 1) sqrt2-mult-c))))

(define-vug butter-bp (in fcut bandwidth)
  "Second-order Butterworth bandpass filter."
  (with-samples ((c (/ 1.0 (tan (* bandwidth *pi-div-sr*))))
                 (d (* -2.0 (cos (hz->radians fcut)))))
    (biquad in 1 0 -1 (+ c 1) (* c d) (- c 1))))

(define-vug butter-br (in fcut bandwidth)
  "Second-order Butterworth bandreject filter."
  (with-samples ((c (tan (* bandwidth *pi-div-sr*)))
                 (d (* -2.0 (cos (hz->radians fcut)))))
    (biquad in 1 d 1 (+ c 1) d (- 1 c))))

;;; Based on Josep Comajuncosas' 18dB/oct resonant 3-pole LPF with
;;; tanh dist (Csound)
(define-vug lpf18 (in freq resonance distortion)
  "Digital emulation of a 3 pole lowpass filter."
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

;;; Reference: http://www.musicdsp.org/archive.php?classid=3#92
;;;
;;; This filter has five simultaneous outputs stored in a FRAME:
;;; low-pass, high-pass, band-pass, notch and peaking.
;;;
;;; It is stable with RESONANCE from 0 to 0.999 and DRIVE from 0 to 0.1
(define-vug svf (in fcut resonance drive)
  "State Variable Filter."
  (with ((ang (min (* +half-pi+ 0.5)
                   ;; Double sampled.
                   (* fcut +half-pi+ *sample-duration*)))
         (freq (* 2.0 (sin ang)))
         (res (clip resonance +sample-zero+ (sample 0.999999)))
         (k0 (expt (the non-negative-sample res) (sample 0.25)))
         (d0 (* 2.0 (- 1.0 k0)))
         (d1 (- (/ 2.0 freq) (* freq 0.5)))
         (damp (min (sample 2) d0 d1))
         (frame (make-frame 5 :zero-p t)))
    (declare (type sample ang freq res k0 d0 d1 damp) (type frame frame))
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

(define-vug maf (in (max-size positive-fixnum) (size positive-fixnum))
  "Moving Average Filter."
  (macrolet ((reset (index-var curr-sum-var acc-var)
               `(setf ,index-var 0
                      ;; Evan Balster's suggestion to prevent drift.
                      ,acc-var ,curr-sum-var
                      ,curr-sum-var +sample-zero+)))
    (with ((data (make-frame max-size :zero-p t))
           (curr-sum 0.0d0)
           (acc 0.0d0)
           (r-size 1.0d0)
           (old-size 0)
           (index 0)
           (last (progn
                   (when (< size old-size)
                     ;; Reset the unused values and update the accumulator.
                     (loop for i from size below old-size do
                          (setf (smp-ref data i) +sample-zero+))
                     (reset index curr-sum acc))
                   (prog1 (1- (setf old-size (min size max-size)))
                     (setf r-size (/ (sample old-size)))))))
      (declare (type pointer data) (type sample curr-sum acc r-size)
               (type non-negative-fixnum index old-size last))
      (incf curr-sum in)                ; Add the new and
      (decf acc (smp-ref data index))   ; subtract the oldest
      (setf (smp-ref data index) in)
      (cond ((< index last)
             (incf index)
             (* (+ curr-sum acc) r-size))
            (t
             (reset index curr-sum acc)
             (* acc r-size))))))

;;; Median filter based on James McCartney's Median ugen (SuperCollider).
;;; Added the code to modulate the window size of the filter.
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
          (incf ,pos))))

(define-vug median (in (max-size positive-fixnum) (size positive-fixnum))
  "Median filter."
  (with ((values (without-follow (max-size)
                   (make-frame max-size :zero-p t)))
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
             (type boolean init-pass-p) (type pointer values)
             (type simple-vector ages))
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
  (with-samples ((th (hz->radians freq))
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
