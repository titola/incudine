;; Exit from the included file if the stack for call/return statements
;; is erroneosly shared.
include "call-return-0.rego"

;; Ignored if the stack is corrupted.
0 rego-test-1 110 .5
