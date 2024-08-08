" Quick fuzzy find in pwd, config dir, $VIMRUNTIME and more

nnoremap <localleader>f :edit **/
nnoremap <localleader>g :vimgrep  **<Left><Left><Left>
nnoremap <expr> <localleader>n
      \ $':edit {fnamemodify(stdpath('config'), ":~")}/**/'
nnoremap <expr> <localleader>N
      \ $':vimgrep  {fnamemodify(stdpath('config'), ':~')}/**<C-Left><Left>'
nnoremap <localleader>r :view $VIMRUNTIME/**/
nnoremap <localleader>R :vimgrep  $VIMRUNTIME/**<C-Left><Left>
nnoremap <localleader>s :scriptnames **/
nnoremap <localleader>S
      \ :vimgrep  `=getscriptinfo()->map({_,f->f.name})`<C-Left><Left>

cnoremap <C-Space> <NOP>
cnoremap <C-Space>f **/
cnoremap <C-Space><C-F> **/
cnoremap <expr> <C-Space>n $'{fnamemodify(stdpath('config'), ":~")}/**/'
cnoremap <expr> <C-Space><C-N> $'{fnamemodify(stdpath('config'), ":~")}/**/'
cnoremap <C-Space>r $VIMRUNTIME/**/
cnoremap <C-Space><C-R> $VIMRUNTIME/**/
