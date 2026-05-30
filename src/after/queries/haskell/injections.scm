;; extends

; "shipped" injection queries do not include whamlet
; maybe submit a PR sometime?
(quasiquote
  (quoter) @_name
  (#eq? @_name "whamlet")
  (quasiquote_body) @injection.content
  (#set! injection.language "html"))

(quasiquote
  quoter: (quoter
    (qualified
      module: (module
        (module_id) @c)
      id: (variable) @var))
  body: (quasiquote_body) @injection.content
  (#eq? @c "C")
  (#any-of? @var "exp" "pure" "block")
  (#set! injection.language "c"))

(quasiquote
  quoter: (quoter) @rust-io
  body: (quasiquote_body) @injection.content
  (#eq? @rust-io "rustIO")
  (#set! injection.language "rust"))

(quasiquote
  (quoter) @_name
  (#eq? @_name "whamlet")
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
;     function: (variable) @nvim_exec
;     argument: (literal (string) @injection.content))
;   (#eq? @nvim_exec "nvim_exec")
;   (#set! injection.language "vim")
;   ; bool
;   argument: (_))

; (apply
;   function: (apply
;     function: (qualified
;       module: (_) ; Neovim.API.Blablabla
;       id: (variable) @nvim_exec)
;     argument: (literal (string) @injection.content))
;   (#eq? @nvim_exec "nvim_exec")
;   (#set! injection.language "vim")
;   argument: (_))

