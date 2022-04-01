(go main)

<<pattern-100>>
0 test-pattern 100
return

<<pattern-200>>
0 test-pattern 200
return

main
:score-radix: 16
call [[pattern-100]] 00
call [[pattern-200]] 04
call [[pattern-100]] 08
call [[pattern-200]] 0C
call [[pattern-100]] 10
