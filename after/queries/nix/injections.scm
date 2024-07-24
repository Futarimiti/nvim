;; extends

; shell script injections
; current only support ''indented_string_expression''s
(binding
  attrpath: (attrpath
			  attr: (identifier) @id)
  (#any-of? @id
   "shellInit"
   "interactiveShellInit"
   "promptInit"
   "loginShellInit"
   "script"
   "direnvrcExtra")
  expression: (indented_string_expression
				(string_fragment) @injection.content)
  (#set! injection.language "sh"))

; TODO: homebrew.extraConfig - ruby syntax
; nix.extraOptions - nix
; vim.vimConfig - viml
