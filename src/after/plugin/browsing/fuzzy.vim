" Quick fuzzy find in directories

cnoremap <C-Space> <NOP>

" file
nnoremap <LocalLeader>f :edit **/
nnoremap <LocalLeader>g :vimgrep  **<Left><Left><Left>
cnoremap <C-Space>f **/
cmap <C-Space><C-F> <C-Space>f

" shipped runtime
nnoremap <LocalLeader>r :edit $VIMRUNTIME/**/
nnoremap <LocalLeader>R :vimgrep  $VIMRUNTIME/**<C-Left><Left>
cnoremap <nowait> <C-Space>r $VIMRUNTIME/**/
cmap <nowait> <C-Space><C-R> <C-Space>r

" scriptnames
nnoremap <LocalLeader>s :scriptnames **/
nnoremap <LocalLeader>S
      \ :vimgrep  `=getscriptinfo()->map({_,f->f.name})`<C-Left><Left>

" oldfiles
command
      \ -nargs=1
      \ -complete=customlist,s:BrowseOldfilesComplete
      \ BrowseOldfiles edit <args>

nnoremap <LocalLeader>o :BrowseOldfiles<Space>
nnoremap <LocalLeader>O :vimgrep `=v:oldfiles`<C-Left><Left>

function s:BrowseOldfilesComplete(arg, _line, _pos) abort
  return v:oldfiles->copy()->filter({ _, f -> stridx(f, a:arg) != -1 })
endfunction
