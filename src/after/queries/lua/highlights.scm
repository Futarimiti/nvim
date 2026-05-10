;; extends

(table_constructor
  [
    "{"
    "}"
  ] @punctuation.bracket)

(function_declaration ["function"] @conceal (#set! conceal "λ"))
(function_definition ["function"] @conceal (#set! conceal "λ"))
