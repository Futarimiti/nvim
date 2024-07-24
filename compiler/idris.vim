if exists("current_compiler")
  finish
endif
let current_compiler = "idris"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=idris
CompilerSet errorformat=
      \%A%f:%l:%c:,
      \%A%f:%l:%c-%k:,
      \%A%f:%l:%c-%e:%k:,
      \%-C\ %#\|,
      \%-C%l\ \|\ %.%#,
      \%-C\ %#\|\ %#%.%#,
      \%C\ %#%m,
      \%-G%.%#,

let &cpo = s:cpo_save
unlet s:cpo_save

