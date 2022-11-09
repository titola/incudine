with (x 1) (y 0)
;; Multiple declarations.
(declare (bit y))
(declare (bit x))

0 type-test 'x (handler-case (incf x) (type-error () 'error))
0 type-test 'y (handler-case (incf y) (type-error () 'error))
