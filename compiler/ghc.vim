if exists("current_compiler")
  finish
endif
let current_compiler = "ghc"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=
    \%-G%.%#:\ build,
    \%-G%.%#preprocessing\ library\ %.%#,
    \%-G[%.%#]%.%#,
    \%E%f:%l:%c:\ %m,
    \%-G--%.%#
CompilerSet makeprg=ghc\ %:S

let &cpo = s:cpo_save
unlet s:cpo_save
