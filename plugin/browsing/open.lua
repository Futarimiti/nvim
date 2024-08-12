-- %:h
vim.keymap.set(
  'n',
  '<leader>o',
  function() vim.ui.open(vim.fs.dirname(vim.api.nvim_buf_get_name(0))) end
)

-- cwd
vim.keymap.set('n', '<leader>O', function() vim.ui.open(vim.fn.getcwd(0)) end)
