;; extends

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
