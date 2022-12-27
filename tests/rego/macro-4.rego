#+begin_macro call-return-test
call p1 5
2 rego-test-1 220 .12
return
12345 rego-test-1 12345 12345
p1
0 rego-test-1 440 .25
return
#+end_macro

;; Exit if the stack for call/return statements is erroneosly shared.
call-return-test

;; Ignored if the stack is corrupted.
0 rego-test-1 110 .5
