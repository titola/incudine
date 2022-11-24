with (x 1) (y 0)
;; Multiple declarations.
(declare (bit y))
(declare (bit x))

;; SBCL compiler: "derived type of (+ 1 X) is (INTEGER 2 2) conflicting
;; with its asserted type BIT". The AMP argument of the first REGO-TEST-1
;; is zero when the rego file is compiled.
0 rego-test-1 123 (* 1/4 (handler-case (incf x) (type-error () 0)))

;; Here the AMP argument is 1/4
1 rego-test-1 123 (* 1/4 (handler-case (incf y) (type-error () 0)))
