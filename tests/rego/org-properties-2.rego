* score
  :PROPERTIES:
  :SCORE-BINDINGS: (t1 4) (t2 8) (curve :exp)
  :SCORE-TEMPO: '(60 120) '(4) :curve curve
  :SCORE-TIME: 0
  :END:

0.0 rego-test-2 440 .3 .5
1.3 rego-test-2 880 .3 .5
2.0 set-control 1 :freq 200
2.0 set-control 2 :freq 208
5.7 set-control 1 :freq 220
5.7 set-control 2 :freq 231

t1 set-controls 1 :freq 300 :amp .1 :pos .1
t1 set-controls 2 :freq 312 :amp .1 :pos .9

:SCORE-BINDINGS: ignored

t2 free 0
