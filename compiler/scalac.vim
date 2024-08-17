if exists('current_compiler')
  finish
endif
let current_compiler = 'scalac'

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=scalac
CompilerSet errorformat=
      \%E\ %#[error]\ %f:%l:%c:\ %m,%C\ %#[error]\ %p^,%-C%.%#,%Z,
      \%W\ %#[warn]\ %f:%l:%c:\ %m,%C\ %#[warn]\ %p^,%-C%.%#,%Z,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
