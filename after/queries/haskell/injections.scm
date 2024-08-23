;; extends

; "shipped" injection queries do not include whamlet
; maybe submit a PR sometime?
(quasiquote
  (quoter) @_name
  (#any-of? @_name "whamlet")
  (quasiquote_body) @injection.content
  (#set! injection.language "html"))
