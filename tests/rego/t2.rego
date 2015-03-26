(tempo 98)

0   rego-test-2 440 .3 .5 :id 1

(setf time 1.3)

0 rego-test-2 880 .3 .5 :id 2

(incf time .7)

0 set-control 1 :freq 200
0 set-control 2 :freq 208

(incf time 3.7)

0 set-control 1 :freq 220
0 set-control 2 :freq 231

(setf time 4)

0 set-controls 1 :freq 300 :amp .1 :pos .1
0 set-controls 2 :freq 312 :amp .1 :pos .9

(incf time 4)

0 free 0
