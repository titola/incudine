#+begin_macro t3
with (x "test-1") (y '(1 2 3)) (result 'failed)

0 list x y free-variable result
#+end_macro

t3 0 (free-variable nil) (result 'ok)
t3 3/2 (x "test-2") (free-variable "...") (result 'ok)
t3 3 "test-3" '(3 2 1) 'ok (free-variable t)
t3 4 "test-4" (result 'ok) (free-variable 'x)
t3 (+ 3/2 (length "beat")) \
   (x "test-5") \
   (free-variable (+ 1 1)) \
   (result 'ok)
