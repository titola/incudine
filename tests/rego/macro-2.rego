with (last 100) ; shadowed in score macro loop-1

(tempo 240)

#+begin_macro loop-1
with (id 1) (last 4)

0          rego-test-1 440 .2 :id id
1          rego-test-1 448 .2 :id (+ id 1)
(1- last)  rego-test-1 661 .2 :id (+ id 2)
last       free 0
#+end_macro

loop-1 0
loop-1 5.1
loop-1 10.1
loop-1 15.1

;; The variable LAST in loop-1 is local.
(- last 94.9)  rego-test-1  220  .2 :id (- last 3)
(- last 84.9)  rego-test-1  220  .2 :id (- last 3)
