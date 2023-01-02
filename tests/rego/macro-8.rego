#+begin_macro m1
with (x 0) (dur (dur x))
0 start-m1-test dur
1 test 'm1 (dur 1) // end-m1-test
#+end_macro

#+begin_macro m2
with dur
(tempo 180)
0 start-m2-test dur
1 test 'm2 (dur 1) // m1 1 // end-m2-test
#+end_macro

#+begin_macro m3
with dur
(tempo 120)
0 start-m3-test dur
1 test 'm3 (dur 1) // m2 (dur (dur 1))
3 m2 (dur 1) (info '(here DUR is not a function but a variable))
4 end-m3-test
#+end_macro

1 m3 1
123 end-test
