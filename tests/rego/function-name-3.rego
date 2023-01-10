* Score functions recursively scheduled
:PROPERTIES:
:score-bindings: (n (or (pop score-args) 12345)) \
                 (m (or (pop score-args) 16)) \
                 (local-call-p (pop score-args)) \
                 (freq (* m 100)) \
                 (next (if score-realtime-p 1/8 1/4)) \
                 (last (if score-realtime-p (- 4 1/8) 4))
:score-function-name: rego-function-name-test-1
:score-local-function-name: self
:score-tempo: 240
:score-realtime-offset: (tempo-sync #[1/4 beats tempo-env 0])
:END:

** Recursion of SELF

(unless (> m 0) (go end-of-score))

0 rego-test-3 (* freq (expt 2 3/12)) (/ m 20) (dur 1/5)
next self n (1- m) t

(if local-call-p (go end-of-score))

** Recursion of REGO-FUNCTION-NAME-TEST-1

(unless (> n 0) (go end-of-score))

last rego-function-name-test-1 (1- n) 16

(if (< n 12345) (go end-of-score))

** New definition for REGO-FUNCTION-NAME-TEST-1

;; The definition of REGO-FUNCTION-NAME-TEST-1 changes after 3 cycles
;; but the recursive local function SELF fills the fourth cycle.
(* 4 3) .1 (lambda () (setf (symbol-function 'rego-function-name-test-1) (constantly nil)))

** End

end-of-score
