with (n 12345)
:score-package: incudine-tests/score-package-2
0 i3 'ma '(dha ni)
;; The package of the symbol N for the binding (N 4) in "package-1.rego" is
;; the current INCUDINE-TESTS/SCORE-PACKAGE-2. The value of INCUDINE-TESTS::N
;; is 12345.
include "package-1.rego" 1
;; The current package remains INCUDINE-TESTS/SCORE-PACKAGE-2
8 i3 'ma '(dha ni)
;; The package of the symbol N for the binding (N 4) in "package-2.rego" is
;; INCUDINE-TESTS/SCORE-PACKAGE-1.
include "package-2.rego" 9
incudine-tests::n 5 i3 'sa '(ni ma)
