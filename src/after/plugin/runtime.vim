packadd vim-scriptease

" :Vedit the file under cursor (like gf)
" default gV is almost useless
nnoremap gV :Vedit <C-R><C-F><CR>
" XXX pollutes "9
xnoremap gV "9y:Vedit <C-R>9<CR>

nnoremap <LocalLeader>v :Vedit **/
