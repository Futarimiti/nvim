" ftplugin/sbt sources ftplugin/scala
if &l:filetype !=# 'scala'
  finish
endif

if findfile('build.sbt', '.') isnot ''
  " sbt must be run in the directory containing sbt.build
  " otherwise there would be strange problems
  let b:start = 'sbt compile --no-colors'
  let b:dispatch = 'sbt console --no-colors'
  compiler sbt
else
  let b:dispatch = 'scalac -color never %'
  let b:start = 'scala'
  compiler scalac
endif
