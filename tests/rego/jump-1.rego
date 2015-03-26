with (n 0.0) (frq 220)

;; From 60 to 180 bpm in 6 beats with sinusoidal curvature
(tempo '(60 180) '(6) :curve :sin)

start
n  rego-test-3  (incf frq 220)  0.3  (dur 0.3)
(if (< (incf n 0.5) 7) (go start))
