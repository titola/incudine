#+begin_macro nested-macro-test
with (n 0) (frq 0) (frq-inc 0) \
     (finc-list '(220 340 114))

(tempo '(60 180) '(6) :curve :sin)

#+begin_macro jump
with (n 0.0)

start
n  rego-test-3  (incf frq frq-inc)  0.3  (dur 0.3)
(if (< (incf n 0.5) 7) (go start))
#+end_macro

start    ; this label is shadowed in score macro `jump'

(setf frq 0 frq-inc (nth n finc-list))

jump (* n 0.05)

(if (< (incf n) 3) (go start))
#+end_macro

nested-macro-test 0 (finc-list '(300 400 200))
