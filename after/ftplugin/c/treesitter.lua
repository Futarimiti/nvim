-- other filetypes without treesitter support
-- e.g. cpp may source c ftplugin
if vim.bo.filetype ~= 'c' then return end

vim.treesitter.start()

vim.opt_local.foldmethod = 'expr'
vim.opt_local.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
