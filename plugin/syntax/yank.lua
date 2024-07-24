vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'vim.highlight.on_yank',
  group = vim.api.nvim_create_augroup('highlight-yank', {}),
  callback = function() vim.highlight.on_yank { higroup = 'Visual', timeout = 200 } end,
})
