" adapted from https://github.com/sheerun/vim-polyglot/blob/f5393cfee07aeb666f4d75f9b3a83163862fb094/compiler/gradlew.vim
if exists("current_compiler")
  finish
endif
let current_compiler = "gradlew"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=./gradlew
CompilerSet errorformat=
      \%E[ant:scalac]\ %f:%l:\ error:\ %m,
      \%W[ant:scalac]\ %f:%l:\ warning:\ %m,
      \%E%.%#:compile%\\w%#Java%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,
      \%W%.%#:compile%\\w%#Java%f:%l:\ warning:\ %m,%-Z%p^,%-C%.%#,
      \%E%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,
      \%W%f:%l:\ warning:\ %m,%-Z%p^,%-C%.%#,
      \%E%f:\ %\\d%\\+:\ %m\ @\ line\ %l\\,\ column\ %c.,%-C%.%#,%Z%p^,
      \%E%>%f:\ %\\d%\\+:\ %m,%C\ @\ line\ %l\\,\ column\ %c.,%-C%.%#,%Z%p^,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
