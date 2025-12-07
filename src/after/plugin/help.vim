nnoremap <LocalLeader>h <CMD>helpclose<CR>

packadd cmdalias.vim
Alias hg helpgrep

" helpgrep
nnoremap gK :helpgrep <C-R><C-W><CR>
" XXX pollutes "9
xnoremap gK "9y:helpgrep <C-R>9<CR>

