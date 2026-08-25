" setlocal shiftwidth=2
" setlocal tabstop=2
" setlocal expandtab
" setlocal textwidth=90
setlocal formatprg=stylish-haskell
setlocal keywordprg=:Stackage

" haskellcomplete#Complete only completes language extension names
" I feel that's more specific to a particular idea rather than ft-specific
" hence move to completefunc instead
setlocal completefunc=haskellcomplete#Complete
setlocal omnifunc=
