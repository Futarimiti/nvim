vim.api.nvim_set_hl(
  0,
  'YankHighlight',
  { bg = 'NvimLightBlue', fg = '#0000FF' }
)
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'vim.highlight.on_yank',
  group = vim.api.nvim_create_augroup('highlight-yank', {}),
  callback = function()
    vim.hl.on_yank { higroup = 'YankHighlight', timeout = 200 }
  end,
})
