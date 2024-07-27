setlocal textwidth=100
setlocal formatprg=astyle
setlocal foldmethod=indent

if !findfile('pom.xml', '.;')->empty()
  compiler maven
else
  compiler javac
endif
