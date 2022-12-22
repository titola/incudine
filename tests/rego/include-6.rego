include "t3.rego" 0 (free-variable nil) (result 'ok)
include "t3.rego" 3/2 (x "test-2") (free-variable "...") (result 'ok)
include "t3.rego" 3 "test-3" '(3 2 1) 'ok (free-variable t)
include "t3.rego" 4 "test-4" (result 'ok) (free-variable 'x)
include "t3.rego" \
        (+ 3/2 (length "beat")) \
        (x "test-5") \
        (free-variable (+ 1 1)) \
        (result 'ok)
