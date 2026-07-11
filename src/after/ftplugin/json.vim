lua vim.treesitter.start()
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
