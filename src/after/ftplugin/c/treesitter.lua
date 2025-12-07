-- other filetypes without treesitter support
-- e.g. cpp may source c ftplugin
if vim.bo.filetype ~= 'c' then return end

vim.treesitter.start()

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
