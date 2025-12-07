runtime! compiler/nix.vim
let current_compiler = "nix-build"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=nix-build

let &cpo = s:cpo_save
unlet s:cpo_save
