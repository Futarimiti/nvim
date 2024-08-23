if exists("current_compiler")
  finish
endif
let current_compiler = "lake"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=
      \%t%*[^:]:\ %f:%l:%c:\ %m,
CompilerSet makeprg=lake

let &cpo = s:cpo_save
unlet s:cpo_save
