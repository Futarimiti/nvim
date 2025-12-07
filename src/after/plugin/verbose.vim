packadd vim-scriptease

" NOTE After #27855 prefer g<
" Example usage:
" * You enter :hi<Enter> or whatever command that talks a lot
" * *the screen explodes with burst of output*
" * "That's bad, I should probably now do :Verbose hi"
" * No you just need to :VV
command VV silent noautocmd normal! :<Up><Up><Home>Verbose<Space><CR>
