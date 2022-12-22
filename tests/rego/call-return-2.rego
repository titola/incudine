* score
** sequence

| call | p1                 |   |
| call | p3                 | 1 |
| call | [[p2]]                 | 2 |
| call | [[p4]]                 | 3 |
| call | [[p5][Pattern V]]          | 3 |
| call | [[p6][Pattern number six]] | 4 |

return

** pattern 1
p1
0.00 rego-test-3 114 0.3 0.3
0.05 rego-test-3 228 0.3 0.3
0.10 rego-test-3 342 0.3 0.3
0.50 rego-test-3 456 0.3 0.29
0.55 rego-test-3 570 0.3 0.29
0.60 rego-test-3 684 0.3 0.29
0.98 rego-test-3 798 0.3 0.28
return

** pattern 2
<<p2>>
0.28 rego-test-3 1824 0.3 0.23
0.33 rego-test-3 1938 0.3 0.23
0.38 rego-test-3 2052 0.3 0.22
0.64 rego-test-3 2166 0.3 0.21
0.69 rego-test-3 2280 0.3 0.21
0.74 rego-test-3 2394 0.3 0.21
0.95 rego-test-3 2508 0.3 0.19
return

** pattern 3
p3
0.03 rego-test-3  912 0.3 0.28
0.08 rego-test-3 1026 0.3 0.28
0.45 rego-test-3 1140 0.3 0.27
0.50 rego-test-3 1254 0.3 0.26
0.55 rego-test-3 1368 0.3 0.26
0.88 rego-test-3 1482 0.3 0.25
0.93 rego-test-3 1596 0.3 0.25
0.98 rego-test-3 1710 0.3 0.24
return

** pattern 4
<<p4>>
0.00 rego-test-3 2622 0.3 0.19
0.05 rego-test-3 2736 0.3 0.19
0.22 rego-test-3 2850 0.3 0.18
0.27 rego-test-3 2964 0.3 0.18
0.32 rego-test-3 3078 0.3 0.18
0.45 rego-test-3 3192 0.3 0.17
return

** pattern 5
<<p5>>
0.50 rego-test-3 3306 0.3 0.17
0.55 rego-test-3 3420 0.3 0.16
0.65 rego-test-3 3534 0.3 0.16
0.70 rego-test-3 3648 0.3 0.16
0.75 rego-test-3 3762 0.3 0.15
0.83 rego-test-3 3876 0.3 0.15
0.88 rego-test-3 3990 0.3 0.15
0.93 rego-test-3 4104 0.3 0.15
return

** pattern 6
<<p6>>
0.00 rego-test-3 4218 0.3 0.14
0.05 rego-test-3 4332 0.3 0.14
0.10 rego-test-3 4446 0.3 0.14
0.17 rego-test-3 4560 0.3 0.14
0.22 rego-test-3 4674 0.3 0.13
0.27 rego-test-3 4788 0.3 0.13
return
