" I wish there is a builtin option for this

augroup cmdline-aucomplete
  autocmd!
  autocmd CmdlineChanged [:\/\?\@\=] call wildtrigger()
augroup END

set wildmode=noselect:lastused,full
set wildoptions=pum,fuzzy

" arrow key functions in wildmode have been counterintuitive
cnoremap <expr> <Up> wildmenumode() ? "\<C-E>\<Up>" : "\<Up>"
" cnoremap <expr> <Down> wildmenumode() ? "\<C-E>\<Down>" : "\<Down>"
cnoremap <expr> <Left> wildmenumode() ? "\<C-E>\<Left>" : "\<Left>"
cnoremap <expr> <Right> wildmenumode() ? "\<C-E>\<Right>" : "\<Right>"

" remap <C-P> and <C-N> to <Up> and <Down> when not in wildmode
cnoremap <expr> <C-P> wildmenumode() ? "\<C-P>" : "\<Up>"
cnoremap <expr> <C-N> wildmenumode() ? "\<C-N>" : "\<Down>"
