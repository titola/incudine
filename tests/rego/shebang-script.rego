#!/usr/bin/env -S incudine -R -s
(setq *standard-output* *error-output*)
(setf (symbol-function 'p) #'write-line)
0 p "one"
1 p "two"
2 p "three"
