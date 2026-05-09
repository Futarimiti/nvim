let g:slime_no_mappings = 1
let g:slime_target = 'neovim'
let g:slime_haskell_ghci_add_let = 0

packadd vim-slime

xmap <C-C> <Plug>SlimeRegionSend
nmap <C-C> <Plug>SlimeMotionSend
nmap <C-C><C-C> <Plug>SlimeLineSend
nmap <C-C><Space> :SlimeSend1<Space>
nmap <C-C><C-Space> :SlimeSend1<Space>
nmap <C-C>v <Plug>SlimeConfig
nmap <C-C><C-V> <C-C>v

" disable nmap in cmdwin
autocmd CmdwinEnter * nnoremap <buffer> <nowait> <C-C> <C-C>
