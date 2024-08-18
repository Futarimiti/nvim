if findfile('pom.xml', '.;') isnot ''
  let b:dispatch = 'mvn --color never compile'
  compiler maven
else
  let b:dispatch = 'javac %'
  let b:start = 'java %'
  compiler javac
endif
