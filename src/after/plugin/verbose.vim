packadd vim-scriptease

" Redo last command but with :Verbose
" If you just want to retrieve the last output in a window, use g<
command VV silent noautocmd normal! :<Up><Up><Home>Verbose<Space><CR>
