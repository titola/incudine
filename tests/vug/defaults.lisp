(in-package :incudine-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-vug vug-default-test-1 (freq amp (nh fixnum))
    (:defaults 110 .3 (+ 1 (random 10)))
    (buzz freq amp nh))

  (define-vug vug-default-test-2 (amp (lst cons))
    (:defaults .5 (list 440))
    (sine (reduce-warnings (sample (car lst))) amp 0))

  (define-vug-macro vug-macro-default-test-2 (freq amp)
    (:defaults 440 .3)
    `(sine ,freq ,amp 0))

  (define-ugen ugen-default-test-1 sample (in fcut q)
    (:defaults 0 2000 100)
    (resonz in fcut q)))

(dsp! default-test-1 (amp (nh fixnum) res)
  (:defaults .3 20 3.9)
  (vuglet ((lp (in fcut res)
             (:defaults 0 1000 0)
             (moogff in fcut res nil))
           (hp (in fcut)
             (:defaults 0 8000)
             (butter-hp in fcut))
           (noise ()
             (noise-test amp)))
    (out (+ (lp (noise) :res res)
            (hp (noise))
            (ugen-default-test-1 (noise)))
         (+ (vug-default-test-1 :nh nh)
            (vug-macro-default-test-2 :amp amp)))))

(dsp! default-test-2 ()
  ;; Explicit defaults for VUG-FUNCALL to parse (list 440) otherwise
  ;; the generated code is an unquoted (440) and the compilation fails.
  (out (vug-default-test-2)))

(with-dsp-test (defaults.1 :channels 2
      :md5 #(66 12 5 72 208 226 193 134 68 106 21 60 138 81 161 114))
  (default-test-1))

(with-dsp-test (defaults.2
      :md5 #(13 165 85 102 64 98 53 34 161 88 38 4 70 123 22 209))
  (default-test-2))
