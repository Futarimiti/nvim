setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal comments=:--
setlocal commentstring=--\ %s

if findfile('stack.yaml', '.;') isnot ''
  let b:start = 'stack ghci'
  let b:dispatch = 'stack build'
  compiler stack
else
  let b:dispatch = 'cabal repl'
  let b:start = 'cabal build'
  compiler cabal
endif
