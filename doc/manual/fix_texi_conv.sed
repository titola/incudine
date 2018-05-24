# PDF A4
/^@settitle/a @afourwide

/^@contents/r macros.texi

# Don't start tables at the bottoms of printed pages.
# A table requires at least 4 cm.
/^@multitable/i @need 1600

# Original enumeration for fdl.
/@appendix GNU Free Doc/,/@enumerate/ {
    s/^Version/@center &/
    s/\(@enumerate\)[ \t]*$/\1 0/
}
/^MODIFICATIONS/,/@enumerate/ {
    s/\(@enumerate\)[ \t]*$/\1 A/
}
