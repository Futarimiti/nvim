vim.keymap.set('n', 'zS', vim.show_pos, { desc = 'vim.show_pos' })
vim.keymap.set('n', '<LocalLeader>i', 'zS', { remap = true })
vim.keymap.set(
  'n',
  '<LocalLeader>I',
  vim.treesitter.inspect_tree,
  { desc = 'vim.treesitter.inspect_tree' }
)
