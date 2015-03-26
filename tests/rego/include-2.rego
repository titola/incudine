with (last 100) ; shadowed in "loop-1.rego"

(tempo 240)

include "loop-1.rego" 0
include "loop-1.rego" 5.1
include "loop-1.rego" 10.1
include "loop-1.rego" 15.1

;; The variable LAST in "loop-1.rego" is local.
(- last 94.9)  rego-test-1  220  .2 :id (- last 3)
(- last 84.9)  rego-test-1  220  .2 :id (- last 3)
