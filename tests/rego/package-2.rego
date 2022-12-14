:score-package: incudine-tests/score-package-1
with (n 4) \
     (incudine-tests/score-package-2::n 2)
0 i2 're '(mi fa)
1 i2 're '(mi fa)
2 i2 're '(mi fa)
:score-package: incudine-tests/score-package-2
n 1 i3 'dha '(ni sa)
(+ n 2) i3 'dha '(ni sa)
:score-package: incudine-tests/score-package-1
n 1 i2 're '(mi fa)
(+ n 2) i2 're '(mi fa)
(+ n 3) incudine-tests::i1 'incudine-tests::a '(incudine-tests::b incudine-tests::c)
