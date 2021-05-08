* Test for floating-point number
** first tests

| 0 | float-test |      1.0 |    1.2345 |
| 0 | float-test | 1.125s-3 | 123.456d0 |
| 0 | float-test |     1f-3 |      1d-3 |
| 0 | float-test |     1e-3 |   1.234l0 |

:score-float-format: single-float

| 1 | float-test |      1.0 |    1.2345 |
| 1 | float-test | 1.125s-3 | 123.456d0 |
| 1 | float-test |     1f-3 |      1d-3 |
| 1 | float-test |     1e-3 |   1.234l0 |

** last test
   :PROPERTIES:
   :score-float-format: double-float
   :END:

| 2 | float-test |      1.0 |    1.2345 |
| 2 | float-test | 1.125s-3 | 123.456d0 |
| 2 | float-test |     1f-3 |      1d-3 |
| 2 | float-test |     1e-3 |   1.234l0 |
