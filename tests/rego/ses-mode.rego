   0.00000  rego-test-1  440.00   0.100
   1.00000  rego-test-1  660.00   0.200
   2.00000  rego-test-1  880.00   0.300


(ses-cell A1 0 nil nil nil)
(ses-cell B1 rego-test-1 'rego-test-1 nil nil)
(ses-cell C1 440 nil nil nil)
(ses-cell D1 0.1 nil nil nil)

(ses-cell A2 1 nil nil nil)
(ses-cell B2 rego-test-1 'rego-test-1 nil nil)
(ses-cell C2 660 nil nil nil)
(ses-cell D2 0.2 nil nil nil)

(ses-cell A3 2 nil nil nil)
(ses-cell B3 rego-test-1 'rego-test-1 nil nil)
(ses-cell C3 880 nil nil nil)
(ses-cell D3 0.3 nil nil nil)

(ses-column-widths [10 12 7 7])
(ses-column-printers ["%.5f" nil "%.2f" "%.3f"])
(ses-default-printer ("%.7g"))
(ses-header-row 0)

( ;Global parameters (these are read first)
 2 ;SES file-format
 3 ;numrows
 4 ;numcols
)

;; Local Variables:
;; mode: ses
;; End:
