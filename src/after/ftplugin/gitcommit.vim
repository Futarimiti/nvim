" make sure rhubarb.vim is loaded
packadd vim-rhubarb

" delete the autocmd that keeps setting omnifunc
autocmd! rhubarb FileType gitcommit

setlocal omnifunc=
setlocal completefunc=rhubarb#Complete
