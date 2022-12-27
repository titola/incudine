#+begin_macro macro-recursive-loop-test-1
0 rego-test-2 880 .2 .2
0 rego-test-2 660 .2 .8

;; Error: loop of recursive score macros.
macro-recursive-loop-test-2
#+end_macro

#+begin_macro macro-recursive-loop-test-2
macro-recursive-loop-test-1

2 rego-test-2 440 .3 0
2 rego-test-2 447 .3 1
#+end_macro

macro-recursive-loop-test-1
