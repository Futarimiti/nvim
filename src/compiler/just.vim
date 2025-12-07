if exists("current_compiler")
  finish
endif
let current_compiler = "just"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=just

let &cpo = s:cpo_save
unlet s:cpo_save
