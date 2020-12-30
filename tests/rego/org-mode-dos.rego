#+TITLE: Piangere come una vite tagliata (dos text file format)
#+AUTHOR: Dado Del Bullone

* prova
  :PROPERTIES:
  :Rate: 5 stars
  :END:

with (n 2) (frq 660)

;; click on start (all we hear is radio blah blah)

** uno

#+BEGIN: clocktable :maxlevel 2 :emphasize nil :scope file
#+CAPTION:  [2015-08-02 dom 17:11]
|       |        |
|-------+--------|
| *nil* | *0:00* |
#+END: clocktable

*** TODO [#B] due [2/3]                                                :BASS:
    DEADLINE: <2015-08-02 dom>

- [X] set BPM

  (tempo 135)

- [X] biquad section

  $$ H\left(z\right)=g\,{{1+\beta_{1}\,z^ {- 1 }+\beta_{2}\,z^ {- 2}}\over{1+a_{1}\,z^ {- 1 }+a_{2}\,z^ {- 2 }}} $$

- [ ] discover bugs with rego files and org syntax
  + hint: read the journal tomorrow

**** tre

| ! | time    | function    | freq |  amp |   | dB |
|---+---------+-------------+------+------+---+----|
| / | <       |             | >    |    < |   |  > |
| _ | t0      |             |      |      |   |    |
| # | 0       | rego-test-1 | 440  | 0.50 | ; | -6 |
| ^ |         |             | a4   |      |   |    |
| * | 1       | rego-test-1 | 448  | 0.35 | ; | -9 |
| $ | base=10 |             |      |      |   |    |
|---+---------+-------------+------+------+---+----|
#+TBLFM: $2=$t0+1::$4=$a4+8::$5=$base^($dB/20);%.2f

#+BEGIN_QUOTE
The only people for me are the mad ones, the ones who are mad to live,
mad to talk, mad to be saved, desirous of everything at the same time,
the ones who never yawn or say a commonplace thing, but burn, burn,
burn, like fabulous yellow Roman candles exploding like spiders across
the stars.
                -- Jack Kerouac, Dharma Bums
#+END_QUOTE

***** prova

(go :labella)

<<<start>>>
n rego-test-1 (incf frq 110) .03
(if (< (incf n 0.5) 5) (go <<<start>>>))

(go <<<game-over>>>)

#+BEGIN_SRC lilypond :file ly_test.png :exports results
\version "2.19.24"
\relative c' { c e g b }
\paper{
  oddFooterMarkup=##f
  oddHeaderMarkup=##f
}
#+END_SRC

#+RESULTS:
[[file:ly_test.png]]

: small example

:labella
n rego-test-1 2200 .1
(incf n)
(go <<<start>>>)

* TODO the end
  SCHEDULED: <2015-08-03 lun>

<<<game-over>>>
(+ n 2) free 0

* config

[[http://incudine.sf.net][incudine home page]]

#+STARTUP: latexpreview

;;; Local Variables:
;;; mode: org
;;; org-babel-lilypond-nix-ly-path: "/usr/local/bin/lilypond"
;;; End:
