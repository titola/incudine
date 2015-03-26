with (n 0) (frq 0) (frq-inc 0) \
     (finc-list '(220 340 114))

(tempo '(60 180) '(6) :curve :sin)

start    ; this label is shadowed in "jump-2.rego"

(setf frq 0 frq-inc (nth n finc-list))

include "jump-2.rego" (* n 0.05)

(if (< (incf n) 3) (go start))
