setlocal keywordprg=:Man\ 3
setlocal formatprg=astyle
setlocal textwidth=80

if findfile('makefile', '.;')->empty()
  compiler gcc
endif
