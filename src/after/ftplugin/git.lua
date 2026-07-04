-- diff grammar does not parse context like author, time, commit etc
-- still need old syntax
vim.treesitter.start()
vim.bo.syntax = 'ON'

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
