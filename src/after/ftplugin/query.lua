-- treesitter already started in $VIMRUNTIME/ftplugin/query.lua

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
