:score-bindings: (n 4) (incudine-tests/score-package-2::n 2)
0 i1 'a '(b c)
:score-package: incudine-tests/score-package-1
1 i2 're '(mi fa)
2 i2 're '(mi fa)
:score-package: incudine-tests/score-package-2
n 1 i3 'dha '(ni sa)
(+ n 2) i3 'dha '(ni sa)
:score-package: incudine-tests/score-package-1
incudine-tests::n 1 i2 're '(mi fa)
(+ incudine-tests::n 2) i2 're '(mi fa)
