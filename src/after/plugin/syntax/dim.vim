let g:vimade = {}
let g:vimade.basebg = [41, 42, 46]

packadd vimade

VimadeFadeLevel 0.8

" mnemonic: t for 'tint'
nnoremap <silent> <LocalLeader>t <CMD>VimadeToggle<CR>
xnoremap <silent> <LocalLeader>t <CMD>VimadeToggle<CR>
