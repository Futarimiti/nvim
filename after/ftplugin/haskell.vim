setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal textwidth=90
setlocal formatprg=stylish-haskell

if !findfile('stack.yaml', '.;')->empty()
  compiler stack
" cabal must be run in the directory as *.cabal
elseif !glob('*.cabal')->empty()
  compiler cabal
else
  compiler ghc
endif
