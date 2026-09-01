" Quick fuzzy find in directories

cnoremap <C-Space> <NOP>

" file
nnoremap <LocalLeader>f :edit **/<C-E>
nnoremap <LocalLeader>g :vimgrep<Space><Space>**<Left><Left><Left>
cnoremap <C-Space>f **/
cmap <C-Space><C-F> <C-Space>f

" shipped runtime
nnoremap <LocalLeader>r :edit $VIMRUNTIME/**/<C-E>
nnoremap <LocalLeader>R :vimgrep<Space><Space>$VIMRUNTIME/**<C-Left><Left>
cnoremap <nowait> <C-Space>r $VIMRUNTIME/**/
cmap <nowait> <C-Space><C-R> <C-Space>r

" full runtimepath
packadd vim-scriptease
nnoremap <LocalLeader>v :Vedit **/<C-E>
" :Vedit the file under cursor (like gf); default gV is almost useless
nnoremap gV :Vedit **/<C-R><C-F><CR>
" XXX pollutes "9
xnoremap gV "9y:Vedit **/<C-R>9<CR>

" scriptnames
nnoremap <LocalLeader>s :scriptnames<Space>
nnoremap <LocalLeader>S
      \ :vimgrep<Space><Space>`=getscriptinfo()->map({_,f->f.name})`<C-Left><Left>

" oldfiles
command
      \ -nargs=1
      \ -complete=customlist,s:OldfilesComplete
      \ Oldfiles edit <args>

nnoremap <LocalLeader>o :Oldfiles<Space>
nnoremap <LocalLeader>O :vimgrep<Space><Space>`=v:oldfiles`<C-Left><Left>

function s:OldfilesComplete(arg, _line, _pos) abort
  return v:oldfiles->copy()->filter({ _, f -> stridx(f, a:arg) != -1 })
endfunction
