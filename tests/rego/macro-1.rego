(tempo 120)

#+begin_macro loop-1
with (id 1) (last 4)

0          rego-test-1 440 .2 :id id
1          rego-test-1 448 .2 :id (+ id 1)
(1- last)  rego-test-1 661 .2 :id (+ id 2)
last       free 0
#+end_macro

#+begin_macro test-1
i1  0    2.2   440  .3
i1  1    1.35  448  .2
i1  1.5  3     666  .2
i1  2.8  1.95  881  .1
i1  3.2   .88  220  .1
#+end_macro

loop-1
test-1 4.1
test-1-label
6 rego-test-3 1100 .3 (dur 3)
