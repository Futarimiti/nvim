-- treesitter already started once
-- in $VIMRUNTIME/ftplugin/lua.lua

vim.opt_local.foldmethod = 'expr'
vim.opt_local.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
