with (n 0.0)

start

;; The variables FRQ and FRQ-INC are defined in the parent regofile.
n  rego-test-3  (incf frq frq-inc)  0.3  (dur 0.3)
(if (< (incf n 0.5) 7) (go start))
