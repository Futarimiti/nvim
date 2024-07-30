if vim.fn.exepath 'lua-language-server' == '' then return end

vim.lsp.start {
  cmd = { 'lua-language-server' },
  name = 'lua-ls',
  root_dir = vim.fs.root(0, { '.luarc.json' }),
}

-- change this with better, official vim.lsp.complete() after it becomes stable
vim.b.autocomplete_key = '<C-X><C-O>'
