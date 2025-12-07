if exists("current_compiler")
  finish
endif
let current_compiler = "koka"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=koka

let &cpo = s:cpo_save
unlet s:cpo_save
