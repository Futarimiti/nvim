;; extends

; "shipped" injection queries do not include whamlet
; maybe submit a PR sometime?
(quasiquote
  (quoter) @_name
  (#any-of? @_name "whamlet")
  (quasiquote_body) @injection.content
  (#set! injection.language "html"))

; Neovim.API.*.nvim_exec
(apply
  function: (apply
              function: (variable) @nvim_exec
              argument: (quasiquote
                          quoter: (_)
                          body: (quasiquote_body) @injection.content))
  (#eq? @nvim_exec "nvim_exec")
  (#set! injection.language "vim")
  ; Bool
  argument: (_))

(apply
  function: (apply
              function: (qualified
                          module: (_) ; Neovim.API.Blablabla
                          id: (variable) @nvim_exec)
              argument: (quasiquote
                          quoter: (_)
                          body: (quasiquote_body) @injection.content))
  (#eq? @nvim_exec "nvim_exec")
  (#set! injection.language "vim")
  argument: (_))

; not working - cannot yet get the string body
; (apply
;   function: (apply
; 			  function: (variable) @nvim_exec
; 			  argument: (literal (string) @injection.content))
;   (#eq? @nvim_exec "nvim_exec")
;   (#set! injection.language "vim")
;   ; bool
;   argument: (_))

; (apply
;   function: (apply
; 			  function: (qualified
; 						  module: (_) ; Neovim.API.Blablabla
; 						  id: (variable) @nvim_exec)
; 			  argument: (literal (string) @injection.content))
;   (#eq? @nvim_exec "nvim_exec")
;   (#set! injection.language "vim")
;   argument: (_))
;
