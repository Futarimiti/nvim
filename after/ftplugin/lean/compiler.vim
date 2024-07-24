if findfile('lakefile.lean', '.;') isnot ''
  let b:dispatch = 'lake build --no-ansi'
  let b:start = '-wait=always lake exe ' .. getcwd()->fnamemodify(':t')
  compiler lake
else
  let b:dispatch = 'lean %'
  let b:start = 'lean --run %'
  compiler lean
endif

