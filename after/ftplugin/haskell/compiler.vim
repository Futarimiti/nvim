if findfile('stack.yaml', '.;') isnot ''
  let b:dispatch = 'stack build --color never'
  let b:start = 'stack ghci'
  compiler stack
elseif glob('*.cabal', v:true) isnot ''
  " https://github.com/haskell/cabal/issues/9188
  let b:dispatch = 'cabal build --ghc-options=-fdiagnostics-color=never'
  let b:start = 'cabal repl'
  compiler cabal
elseif executable('ghc')
  let b:dispatch = 'ghc -fdiagnostics-color=never %'
  let b:start = 'ghci %'
  compiler ghc
elseif executable('stack')
  let b:dispatch = 'stack ghc -- -fdiagnostics-color=never %'
  let b:start = 'stack ghci %'
  compiler stack
else
  let b:dispatch = 'ghc -fdiagnostics-color=never %'
  let b:start = 'ghci %'
  compiler ghc
endif
