0 rego-test-2 880 .2 .2
0 rego-test-2 660 .2 .8

;; Error: loop of included rego files.
include "include-loop-1.rego"
