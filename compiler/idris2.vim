if exists("current_compiler")
  finish
endif
let current_compiler = "idris2"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=idris2
CompilerSet errorformat=
      \%.%#/%.%#:\ Building\ %o\ (%f),
      \%EError:\ %m,
      \%C,
      \%C%o:%l:%c--%e:%k,
      \%C\ %#%l\ %#\|\ %#%.%#,
      \%C%p^,

let &cpo = s:cpo_save
unlet s:cpo_save
