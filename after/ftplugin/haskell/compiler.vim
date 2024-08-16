if findfile('stack.yaml', '.;') isnot ''
  let b:start = 'stack ghci'
  let b:dispatch = 'stack build'
  compiler stack
elseif glob('*.cabal', v:true) isnot ''
  " cabal must be run in the directory as *.cabal
  " ignore 'wildignore'
  let b:dispatch = 'cabal repl'
  let b:start = 'cabal build'
  compiler cabal
elseif executable('ghc')
  let b:dispatch = 'ghc %'
  let b:start = 'ghci %'
  compiler ghc
elseif executable('stack')
  let b:start = 'stack ghci %'
  let b:dispatch = 'stack ghc %'
  compiler stack
else
  let b:dispatch = 'ghc %'
  let b:start = 'ghci %'
  compiler ghc
endif
