:score-bindings: (n (or (first score-args) 10)) \
                 (freq (* n 180)) \
                 (last (if score-realtime-p 3 4))
:score-function-name: rego-function-name-test-1
:score-tempo: '(200 600) '(4) :curve -2.88
:score-realtime-offset: (tempo-sync #[4 beats tempo-env 0])

0 rego-test-3    freq      3/4 (dur 1/2)
1 rego-test-3 (* freq 5/4) 3/4 (dur 1/2)
2 rego-test-3 (* freq 3/2) 3/4 (dur 1/2)
3 rego-test-3 (* freq 7/4) 3/4 (dur 1/2)

(unless (> n 0) (go end-of-score))

last rego-function-name-test-1 (1- n)

end-of-score
