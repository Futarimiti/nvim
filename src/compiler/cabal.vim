if exists("current_compiler")
  finish
endif
let current_compiler = "cabal"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=
    \%E%f:%l:%c:\ error:,%+Z\ \ \ \ %m,
    \%E%f:%l:%c:\ error:\ %m,%-Z,
    \%W%f:%l:%c:\ warning:,%+Z\ \ \ \ %m,
    \%W%f:%l:%c:\ warning:\ %m,%-Z,
CompilerSet makeprg=cabal

let &cpo = s:cpo_save
unlet s:cpo_save
