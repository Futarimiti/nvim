" ftplugin/sbt sources ftplugin/scala
if &l:filetype !=# 'scala'
  finish
endif

if glob('build.sbt', v:true) isnot ''
  " sbt must be run in the directory containing sbt.build
  " otherwise there would be strange problems
  let b:dispatch = 'sbt compile --no-colors'
  let b:start = 'sbt console'
  compiler sbt
else
  let b:dispatch = 'scalac -color never %'
  let b:start = 'scala'
  compiler scalac
endif
