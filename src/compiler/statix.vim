if exists("current_compiler")
  finish
endif
let current_compiler = "statix"

let s:cpo_save = &cpo
set cpo&vim

" CompilerSet makeprg=statix\ check\ -o\ errfmt\ %
CompilerSet makeprg=statix
CompilerSet errorformat=%f>%l:%c:%t:%n:%m

if !exists(':Statix')
  if exists(':Make') == 2
    command -bang Statix Make<bang> check -o errfmt %
  else
    command -bang Statix make<bang> check -o errfmt %
  endif
endif

let &cpo = s:cpo_save
unlet s:cpo_save
