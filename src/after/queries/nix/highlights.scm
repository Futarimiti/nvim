;; extends

;; highlight uri in strings
((string_fragment) @_uri
  (#match?
   @_uri
   "^[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9%\/\?:@\&=\+\$,\-_\.\!\~\*\']+$" ; taken from upstream TS parser
   )) @string.special.path
