vim.treesitter.language.register('latex', 'plaintex')
vim.treesitter.language.register('latex', 'tex')
vim.treesitter.language.register('diff', 'git')
vim.treesitter.language.register('html', 'xhtml')

-- incremental selection
vim.keymap.set('v', '+', 'an', { remap = true })
vim.keymap.set('v', '-', 'in', { remap = true })

-- text objects
vim.cmd.packadd 'nvim-treesitter-textobjects'
require('nvim-treesitter-textobjects').setup {
  select = { lookahead = true },
  move = { set_jumps = true },
}
-- local move = require 'nvim-treesitter-textobjects.move'
-- local select = require 'nvim-treesitter-textobjects.select'
-- local swap = require 'nvim-treesitter-textobjects.swap'

-- treewalker
vim.cmd.packadd 'treewalker.nvim'
local treewalker = require 'treewalker'
treewalker.setup {}

-- movement
vim.keymap.set(
  { 'n', 'v' },
  'gmk',
  treewalker.move_up,
  { desc = 'Treewalker Up' }
)
vim.keymap.set(
  { 'n', 'v' },
  'gmj',
  treewalker.move_down,
  { desc = 'Treewalker Down' }
)
vim.keymap.set(
  { 'n', 'v' },
  'gmh',
  treewalker.move_out,
  { desc = 'Treewalker Left' }
)
vim.keymap.set(
  { 'n', 'v' },
  'gml',
  treewalker.move_in,
  { desc = 'Treewalker Right' }
)

-- swapping
vim.keymap.set('n', 'gsk', treewalker.swap_up, { desc = 'Treewalker SwapUp' })
vim.keymap.set(
  'n',
  'gsj',
  treewalker.swap_down,
  { desc = 'Treewalker SwapDown' }
)
vim.keymap.set(
  'n',
  'gsh',
  treewalker.swap_left,
  { desc = 'Treewalker SwapLeft' }
)
vim.keymap.set(
  'n',
  'gsl',
  treewalker.swap_right,
  { desc = 'Treewalker SwapRight' }
)
