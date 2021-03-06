(in-package :incudine-tests)

(enable-sharp-t-syntax)

;;; Moog "Voltage Controlled Filter" (VCF) in "analog" form.
;;; References:
;;;   [1] https://ccrma.stanford.edu/~stilti/papers/moogvcf.pdf
;;;   [2] https://ccrma.stanford.edu/~jos/pasp/vegf.html
(define-vug moog-vcf (in res fr)
  (with-samples ((wt (hz->radians fr))
                 (coef (- 1 wt))
                 (unity-gain (* wt wt wt wt))
                 (mk (* -4 (clip res 0.0d0 0.999999d0))))
    (~ (* #4t(pole (+ in it) coef) unity-gain mk))))

(dsp! moog-vcf-test-1 ()
  (out (moog-vcf (buzz 200 1 100) .99 (expon 80 5000 5 #'free))))

(deftest sharp-t.1
    '#4t(pole (+ in it) coef)
    (POLE (POLE (POLE (POLE (+ IN IT) COEF) COEF) COEF) COEF))

(deftest sharp-t.2
    '#4t1(some-filter x in y)
    (SOME-FILTER X (SOME-FILTER X (SOME-FILTER X (SOME-FILTER X IN Y) Y) Y) Y))

(with-dsp-test (sharp-t.3
      :md5 #+64-bit #(30 218 16 53 229 36 210 214 173 140 82 18 129 212 13 6)
           #-64-bit #(77 208 28 188 203 248 7 149 171 236 1 228 108 206 48 17))
  (moog-vcf-test-1))
