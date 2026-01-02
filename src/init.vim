" must turn on NOW, in init.vim
" otherwise too late to pick up exrcs
set exrc

" neovide
if exists('g:neovide')
  let g:neovide_transparency = 0.85
  let g:neovide_scroll_animation_length = 0.1
  let g:neovide_position_animation_length = 0.05
  let g:neovide_input_macos_option_key_is_meta = 'both'
  let g:neovide_cursor_trail_size = 0.5
endif

" leaders
let mapleader = '\'
let maplocalleader = ' '
nmap , \
xmap , \
omap , \
