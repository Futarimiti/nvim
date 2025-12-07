let b:dispatch = 'lake build --no-ansi'
let b:start = '-wait=always lake exe ' .. getcwd()->fnamemodify(':t')
compiler lake
