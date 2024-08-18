if findfile('stack.yaml', '.;') isnot ''
  let b:start = 'stack ghci'
  let b:dispatch = 'stack build'
  compiler stack
else
  let b:dispatch = 'cabal repl'
  let b:start = 'cabal build'
  compiler cabal
endif
