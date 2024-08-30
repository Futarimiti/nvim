;; extends

; multiway-lambda
; https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst
["cases"] @keyword.conditional

; string-interpolate quasiquoters
(quasiquote
  (quoter) @_name
  (#any-of? @_name "i" "iii" "__i" "__i'E" "__i'L" "iii'E" "iii'L")
  (quasiquote_body) @string.interpolate)

; raw-strings-qq quasiquoters
(quasiquote
  (quoter) @_name
  (#any-of? @_name "r" "rQ")
  (quasiquote_body) @string.interpolate)
