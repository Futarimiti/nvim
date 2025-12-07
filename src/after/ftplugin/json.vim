lua vim.treesitter.start()
setlocal formatprg=json-fmt
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
