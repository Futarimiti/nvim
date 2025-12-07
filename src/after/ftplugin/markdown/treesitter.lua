vim.treesitter.start(0, 'markdown_inline')
vim.treesitter.start(0, 'markdown')

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
