" adapted from
" https://github.com/derekwyatt/vim-scala/blob/master/compiler/sbt.vim

if exists('current_compiler')
  finish
endif
let current_compiler = 'sbt'

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=sbt
CompilerSet errorformat=
      \%E\ %#[error]\ %f:%l:%c:\ %m,%C\ %#[error]\ %p^,%-C%.%#,%Z,
      \%W\ %#[warn]\ %f:%l:%c:\ %m,%C\ %#[warn]\ %p^,%-C%.%#,%Z,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
