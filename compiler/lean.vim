if exists("current_compiler")
  finish
endif
let current_compiler = "lean"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=
      \%f:%l:%c:\ %t%*[^:]:\ %m,
CompilerSet makeprg=lean

let &cpo = s:cpo_save
unlet s:cpo_save
