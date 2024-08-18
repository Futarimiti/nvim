if findfile('makefile', '.;')->empty()
  let b:dispatch = 'gcc % -o %:r'
  compiler gcc
endif
