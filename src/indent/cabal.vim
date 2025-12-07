if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=cabal#get_indent()
setlocal indentkeys=!^F,o,O,<CR>
