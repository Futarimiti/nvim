" Quick fuzzy find in current directory, user config directory and $VIMRUNTIME

nnoremap <localleader>f :edit **/
nnoremap <localleader>g :vimgrep  **<Left><Left><Left>
nnoremap <expr> <localleader>n ':edit ' .. fnamemodify(stdpath('config'), ':~') .. '/**/'
nnoremap <expr> <localleader>N ':vimgrep  ' .. fnamemodify(stdpath('config'), ':~') .. '/**<C-Left><Left>'
nnoremap <localleader>r :view $VIMRUNTIME/**/
nnoremap <localleader>R :vimgrep  $VIMRUNTIME/**<C-Left><Left>
