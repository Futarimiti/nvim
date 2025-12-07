lua vim.treesitter.start()

setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()

" tricky - defs are indent based while function calls ends with )
let b:use_indent = 1
