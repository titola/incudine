;;; Copyright (c) 2013-2022 Tito Latini
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

(defmacro random-sample (a b)
  `(incudine.gen::ran-flat (incudine.external::gsl-random-generator)
                           (sample ,a) (sample ,b)))

(define-vug white-noise (amp)
  "White noise generator with output optionally scaled by AMP."
  (:defaults 1)
  (with-samples ((amp0 (- amp)))
    (random-sample amp0 amp)))

;;; Reference:
;;;
;;;   [1] http://www.firstpr.com.au/dsp/pink-noise
;;;
;;; Based on Phil Burk's code in C.
(define-vug-macro pink-noise (amp number-of-bands)
  "Pink Noise generator using the Gardner method with the James
McCartney's optimization.

The output is optionally scaled by AMP.

NUMBER-OF-BANDS defaults to 20."
  (:defaults 1 20)
  (let* ((max-random-rows 30)
         (number-of-rows (clip number-of-bands 4 30))
         (random-limit32 4294967296)
         (random-limit24 16777216)
         (random-bits 24)
         (mask (1- (ash 1 number-of-rows)))
         (scale (/ (sample 1) (* (1+ number-of-rows)
                                 (ash 1 (1- random-bits)))))
         (pink (gensym "PINK-NOISE")))
    `(vuglet ((,pink (amp)
                (with ((rows (maybe-make-i32-array ,max-random-rows :zero-p t))
                       (counter 0)
                       (rand32 0)
                       (new-random 0)
                       (total 0)
                       (index 0)
                       (mult (* amp ,scale)))
                  (declare (type (unsigned-byte 32) rand32 new-random counter
                                 index)
                           (type (signed-byte 32) total)
                           (type (integer 0 ,max-random-rows) index)
                           (type sample mult))
                  (initialize (dotimes (i ,number-of-rows)
                                (setf (maybe-i32-ref rows i)
                                      (random ,random-limit24))))
                  (setf rand32 (random ,random-limit32))
                  ;; Magnus Jonsson's suggestion
                  (setf counter (logand rand32 ,mask))
                  (unless (zerop counter)
                    (setf index (ctz counter))
                    (setf new-random (ash rand32 -8))
                    (incf total (the (signed-byte 32)
                                  (- new-random (maybe-i32-ref rows index))))
                    (setf (maybe-i32-ref rows index) new-random))
                  (* mult (the (signed-byte 32)
                            (+ total (random ,random-limit24)))))))
       (,pink ,amp))))

;;; Noise generator used in SuperCollider.
(define-vug crackle (param amp init-value)
  "Noise generator based on a chaotic function with scale factor AMP
(1 by default).

The formula is

    y[n] = | param * y[n-1] - y[n-2] - 0.05 |

INIT-VALUE is the initial value of y and defaults to 0.3."
  (:defaults (incudine:incudine-missing-arg "PARAM") 1 .3)
  (* amp (~ (abs (- (* it param) (delay1 it) 0.05)) :initial-value init-value)))

(define-vug-macro rand (&whole whole distribution &key a b c n n1 n2 p alpha
                        beta mu nu nu1 nu2 sigma tt zeta seed)
  "Noise generator with random number DISTRIBUTION.

|--------------------+------------+------------+-----------+-------------|
| distribution       | param 1    | param 2    | param 3   | return type |
|--------------------+------------+------------+-----------+-------------|
| :linear            | :a 0.0     | :b 1.0     |           | double      |
| :high              | :a 0.0     | :b 1.0     |           | double      |
| :triangular        | :a 0.0     | :b 1.0     |           | double      |
| :gauss             | :sigma 1.0 |            |           | double      |
| :gauss-tail        | :a 1.0     | :sigma 1.0 |           | double      |
| :exp               | :mu 1.0    |            |           | double      |
| :laplace           | :a 1.0     |            |           | double      |
| :exppow            | :a 1.0     | :b 1.5     |           | double      |
| :cauchy            | :a 1.0     |            |           | double      |
| :rayleigh          | :sigma 1.0 |            |           | double      |
| :rayleigh-tail     | :a 1.0     | :sigma 1.0 |           | double      |
| :landau            |            |            |           | double      |
| :levy              | :c 1.0     | :alpha 1.0 |           | double      |
| :levy-skew         | :c 1.0     | :alpha 1.0 | :beta 1.0 | double      |
| :gamma             | :a 1.0     | :b 1.0     |           | double      |
| :uni               | :a 0.0     | :b 1.0     |           | double      |
| :lognormal         | :zeta 1.0  | :sigma 1.0 |           | double      |
| :chisq             | :nu 1.0    |            |           | double      |
| :f                 | :nu1 1.0   | :nu2 1.0   |           | double      |
| :t                 | :nu 1.0    |            |           | double      |
| :beta              | :a 1.0     | :b 1.0     |           | double      |
| :pareto            | :a 1.0     | :b 1.0     |           | double      |
| :logistic          | :a 1.0     |            |           | double      |
| :weibull           | :a 1.0     | :b 1.0     |           | double      |
| :gumbel1           | :a 1.0     | :b 1.0     |           | double      |
| :gumbel2           | :a 1.0     | :b 1.0     |           | double      |
| :poisson           | :mu 1.0    |            |           | uint        |
| :bernoulli         | :p 0.5     |            |           | uint        |
| :binomial          | :n 1       |            |           | uint        |
| :negative-binomial | :p 0.5     | :n 1.0     |           | uint        |
| :pascal            | :p 0.5     | :n 1       |           | uint        |
| :geom              | :p 0.5     |            |           | uint        |
| :hypergeom         | :n1 1      | :n2 1      | :tt 1     | uint        |
| :log               | :p 0.5     |            |           | uint        |
|--------------------+------------+------------+-----------+-------------|

See also GEN:ALL-RANDOM-DISTRIBUTIONS and GEN:RAND-ARGS."
  (declare (ignorable a b c n n1 n2 p alpha beta mu nu nu1 nu2 sigma tt zeta))
  (let ((spec (gen::find-rand-func-spec distribution))
        (pl (cddr whole)))
    (destructuring-bind (type-list (lisp-name foreign-name) return-type
                         &rest args)
        spec
      (declare (ignore type-list foreign-name return-type))
      (let ((keys (mapcar (lambda (x) (make-keyword (caar x))) args)))
        (with-gensyms (rng)
          `(with (,@(loop for arg in args
                          for key in keys
                          for value = (or (getf pl key) (cdr arg))
                          collect `(,(caar arg)
                                    (vug-input
                                      ,(if (eq (cadar arg) :double)
                                           `(coerce ,value 'double-float)
                                           value))))
                  (,rng (progn
                          ,@(when seed
                              `((vug-input
                                 (incudine.external::gsl-seed-random-state
                                   ,seed))))
                          (incudine.external::gsl-random-generator))))
             ,@(let ((samples (loop for arg in args
                                    when (eq (cadar arg) :double)
                                    collect (caar arg))))
                 (when samples
                   `((declare (type #+double-samples sample
                                    #-double-samples double-float
                                    ,@samples)))))
             (declare (type foreign-pointer ,rng))
             (,lisp-name ,rng ,@(mapcar #'caar args))))))))

;;;
;;; FRACTAL-NOISE
;;;
;;; References:
;;;
;;;   [1] R. Saletti. A comparison between two methods to generate 1/(f^gamma) noise.
;;;   In Proc. IEEE, volume 74, pp. 1595-1596, November 1986.
;;;
;;;   [2] G. Corsini and R. Saletti. A 1/(f^gamma) power spectrum noise sequence
;;;   generator. IEEE Trans. on Instrumentation and Measurement, 37(4):615-619,
;;;   December 1988.
;;;
;;;   [3] The Sounding Object, edited by Davide Rocchesso and Federico Fontana,
;;;   Edizioni di Mondo Estremo. Chapter 8 by Federico Avanzini, pp. 154-157.
;;;
(declaim (inline fractal-noise-coeff-calc))
(defun fractal-noise-coeff-calc (freq)
  (- (exp (- (* 2.0 (sample pi) freq *sample-duration*)))))

(define-vug-macro fractal-noise (amp beta &key (poles-density 6)
                                 (filter-order 15) (lowest-freq 50))
  "Fractal noise generator implemented as a white noise filtered by a
cascade of FILTER-ORDER (15 by default) filters.

The output is scaled by AMP.

BETA is the spectral parameter related to the fractal dimension (defines
the target 1/f^beta spectrum). Examples:

  |------+-------|
  | beta | noise |
  |------+-------|
  |    0 | white |
  |    1 | pink  |
  |    2 | brown |
  |------+-------|

POLES-DENSITY defaults to 6.

The frequency LOWEST-FREQ of the first pole defaults to 50."
  (with-gensyms (in c1 c2 p z a b sec r-poles-density)
    `(with-samples ((,in (white-noise (sample 1)))
                    (,r-poles-density (vug-input
                                        (reduce-warnings
                                          (/ (sample 1) ,poles-density))))
                    (,c1 (expt (sample 10) ,r-poles-density))
                    (,c2 (expt (sample 10) (* ,beta ,r-poles-density 0.5)))
                    ,@(loop for i from 1 to filter-order
                            for pole = (vug-format-symbol "~A~D" p i)
                            for pole-value = (sample lowest-freq)
                                           then `(* ,(vug-format-symbol
                                                       "~A~D" p (1- i))
                                                    ,c1)
                            for zero = (vug-format-symbol "~A~D" z i)
                            collect `(,pole ,pole-value)
                            collect `(,(vug-format-symbol "~A~D" a i)
                                      (fractal-noise-coeff-calc ,pole))
                            collect `(,zero (* ,pole ,c2))
                            collect `(,(vug-format-symbol "~A~D" b i)
                                      (fractal-noise-coeff-calc ,zero)))
                    ,@(loop for i from 1 to (/ filter-order 2)
                            for j from 1 by 2
                            for input = in then sec-name
                            for sec-name = (vug-format-symbol "~A~D" sec i)
                            for b1 = (vug-format-symbol "~A~D" b j)
                            for b2 = (vug-format-symbol "~A~D" b (1+ j))
                            for a1 = (vug-format-symbol "~A~D" a j)
                            for a2 = (vug-format-symbol "~A~D" a (1+ j))
                            collect `(,sec-name
                                      (biquad ,input 1 (+ ,b1 ,b2) (* ,b1 ,b2) 1
                                              (+ ,a1 ,a2) (* ,a1 ,a2)))))
       (* ,amp ,(let ((last-sec (vug-format-symbol "~A~D" sec
                                                   (floor (/ filter-order 2)))))
                  (if (oddp filter-order)
                      `(biquad ,last-sec 1
                               ,(vug-format-symbol "~A~D" b filter-order)
                               0 1 ,(vug-format-symbol "~A~D" a filter-order) 0)
                      last-sec))))))
