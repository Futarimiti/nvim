runtime! compiler/nix.vim
let current_compiler = "darwin-rebuild"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=darwin-rebuild

let &cpo = s:cpo_save
unlet s:cpo_save
