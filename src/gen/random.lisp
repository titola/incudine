;;; Copyright (c) 2013-2017 Tito Latini
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

(in-package :incudine.gen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *random-number-distributions* nil)

  (unless *random-number-distributions*
    (macrolet
        ((define-random-functions (lst)
           `(progn
              (setf *random-number-distributions* ',lst)
              ,@(mapcar
                 (lambda (entry)
                   (destructuring-bind (type-list (lisp-name foreign-name)
                                        return-type &rest args)
                       entry
                     (declare (ignore type-list))
                     (when foreign-name
                       `(progn
                          (declaim (inline ,lisp-name))
                          ,(if (and (eq incudine.config:*sample-type*
                                        'double-float)
                                    (eq return-type :double))
                               `(cffi:defcfun (,lisp-name ,foreign-name)
                                  ,return-type (rng-type :pointer)
                                  ,@(loop for l in args collect (car l)))
                               `(defun ,lisp-name (rng-type
                                                    ,@(mapcar #'caar args))
                                  (coerce (cffi:foreign-funcall ,foreign-name
                                            :pointer rng-type
                                            ,@(loop for l in args
                                                    append `(,(cadar l)
                                                             ,(caar l)))
                                            ,return-type)
                                          'sample)))))))
                 lst))))
      (define-random-functions
          (((:linear :low :low-pass :lp)
            (ran-low-pass nil)
            :double ((a :double) . 0.0d0) ((b :double) . 1.0d0))
           ((:high :high-pass :hp)
            (ran-high-pass nil)
            :double ((a :double) . 0.0d0) ((b :double) . 1.0d0))
           ((:triangular :tri :triang :mean)
            (ran-triangular nil)
            :double ((a :double) . 0.0d0) ((b :double) . 1.0d0))
           ((:gauss :gaussian)
            (ran-gaussian "gsl_ran_gaussian_ratio_method")
            :double ((sigma :double) . 1.0d0))
           ((:gauss-tail :gaussian-tail)
            (ran-gaussian-tail "gsl_ran_gaussian_tail")
            :double ((a :double) . 1.0d0) ((sigma :double) 1.0d0))
           ((:exp :expon :exponential)
            (ran-exponential "gsl_ran_exponential")
            :double ((mu :double) . 1.0d0))
           ((:laplace :biexp :biexpon :biexponential)
            (ran-laplace "gsl_ran_laplace")
            :double ((a :double) . 1.0d0))
           ((:exppow :exponential-power)
            (ran-exppow "gsl_ran_exppow")
            :double ((a :double) . 1.0d0) ((b :double) . 1.5d0))
           ((:cauchy)
            (ran-cauchy "gsl_ran_cauchy")
            :double ((a :double) . 1.0d0))
           ((:rayleigh)
            (ran-rayleigh "gsl_ran_rayleigh")
            :double ((sigma :double) . 1.0d0))
           ((:rayleigh-tail)
            (ran-rayleigh-tail "gsl_ran_rayleigh_tail")
            :double ((a :double) . 1.0d0) ((sigma :double) . 1.0d0))
           ((:landau)
            (ran-landau "gsl_ran_landau")
            :double)
           ((:levy)
            (ran-levy "gsl_ran_levy")
            :double ((c :double) . 1.0d0) ((alpha :double) . 1.0d0))
           ((:levy-skew)
            (ran-levy-skew "gsl_ran_levy_skew")
            :double ((c :double) . 1.0d0) ((alpha :double) . 1.0d0)
            ((beta :double) . 1.0d0))
           ((:gamma)
            (ran-gamma "gsl_ran_gamma")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:uni :uniform :flat)
            (ran-flat "gsl_ran_flat")
            :double ((a :double) . 0.0d0) ((b :double) . 1.0d0))
           ((:lognormal)
            (ran-lognormal "gsl_ran_lognormal")
            :double ((zeta :double) . 1.0d0) ((sigma :double) . 1.0d0))
           ((:chisq :chi-squared)
            (ran-chisq "gsl_ran_chisq")
            :double ((nu :double) . 1.0d0))
           ((:f :fdist)
            (ran-fdist "gsl_ran_fdist")
            :double ((nu1 :double) . 1.0d0) ((nu2 :double) . 1.0d0))
           ((:t :tdist)
            (ran-tdist "gsl_ran_tdist")
            :double ((nu :double) . 1.0d0))
           ((:beta)
            (ran-beta "gsl_ran_beta")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:logistic)
            (ran-logistic "gsl_ran_logistic")
            :double ((a :double) . 1.0d0))
           ((:pareto)
            (ran-pareto "gsl_ran_pareto")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:weibull)
            (ran-weibull "gsl_ran_weibull")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:gumbel1)
            (ran-gumbel1 "gsl_ran_gumbel1")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:gumbel2)
            (ran-gumbel2 "gsl_ran_gumbel2")
            :double ((a :double) . 1.0d0) ((b :double) . 1.0d0))
           ((:poisson)
            (ran-poisson "gsl_ran_poisson")
            :unsigned-int ((mu :double) . 1.0d0))
           ((:bernoulli)
            (ran-bernoulli "gsl_ran_bernoulli")
            :unsigned-int ((p :double) . 0.5d0))
           ((:binomial)
            (ran-binomial "gsl_ran_binomial")
            :unsigned-int ((p :double) . 0.5d0) ((n :unsigned-int) . 1))
           ((:negative-binomial)
            (ran-negative-binomial "gsl_ran_negative_binomial")
            :unsigned-int ((p :double) . 0.5d0) ((n :double) . 1.0d0))
           ((:pascal)
            (ran-pascal "gsl_ran_pascal")
            :unsigned-int ((p :double) . 0.5d0) ((n :unsigned-int) . 1))
           ((:geom :geometric)
            (ran-geometric "gsl_ran_geometric")
            :unsigned-int ((p :double) . 0.5d0))
           ((:hypergeom :hypergeometric)
            (ran-hypergeometric "gsl_ran_hypergeometric")
            :unsigned-int ((n1 :unsigned-int) . 1) ((n2 :unsigned-int) . 1)
            ((tt :unsigned-int) . 1))
           ((:log :logarithmic)
            (ran-logarithmic "gsl_ran_logarithmic")
            :unsigned-int ((p :double) . 0.5d0)))))

    (defvar *random-distribution-list*
      (mapcar #'car *random-number-distributions*))

    (declaim (inline find-rand-func-spec))
    (defun find-rand-func-spec (type)
      (or (assoc type *random-number-distributions* :test #'member)
          (incudine:incudine-error "Random number distribution ~A not found"
                                   type)))

    (declaim (inline random-distribution-list))
    (defun all-random-distributions ()
      *random-distribution-list*)

    (declaim (inline rand-args))
    (defun rand-args (distribution)
      (cddr (assoc distribution *random-number-distributions* :key #'car)))

    (declaim (inline ran-low-pass))
    (defun ran-low-pass (rng-type a b)
      (min (ran-flat rng-type a b)
           (ran-flat rng-type a b)))

    (declaim (inline ran-high-pass))
    (defun ran-high-pass (rng-type a b)
      (max (ran-flat rng-type a b)
           (ran-flat rng-type a b)))

    (declaim (inline ran-triangular))
    (defun ran-triangular (rng-type a b)
      (* 0.5 (+ (ran-flat rng-type a b)
                (ran-flat rng-type a b))))))

(defmacro rand (&whole whole distribution
                &key a b c n n1 n2 p alpha beta mu nu nu1 nu2 sigma tt zeta seed)
  (declare (ignorable a b c n n1 n2 p alpha beta mu nu nu1 nu2 sigma tt zeta))
  (with-gensyms (rng i c-array size)
    (let ((spec (find-rand-func-spec distribution))
          (pl (cddr whole)))
      (destructuring-bind (type-list (lisp-name foreign-name) return-type
                           &rest args)
          spec
        (declare (ignore type-list foreign-name return-type))
        (let ((keys (mapcar (lambda (x) (make-keyword (caar x))) args)))
          `(let ((,rng (progn
                         ,@(when seed
                             `((incudine.external::gsl-seed-random-state ,seed)))
                         (incudine.external::gsl-random-generator)))
                 ,@(loop for arg in args
                         for key in keys
                         for value = (or (getf pl key) (cdr arg))
                         collect `(,(caar arg)
                                   ,(if (eq (cadar arg) :double)
                                        `(coerce ,value 'double-float)
                                        value))))
             (lambda (,c-array ,size)
               (declare (type foreign-pointer ,c-array)
                        (type non-negative-fixnum ,size))
               (dotimes (,i ,size)
                 (declare #.*standard-optimize-settings*)
                 (setf (smp-ref ,c-array ,i)
                       (,lisp-name ,rng ,@(mapcar #'caar args)))))))))))
