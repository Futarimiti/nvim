-- nvim-treesitter

-- incremental selection
vim.keymap.set('v', '+', 'an', { remap = true })
vim.keymap.set('v', '-', 'in', { remap = true })

vim.cmd.packadd 'nvim-treesitter-legacy'
vim.cmd.packadd 'nvim-treesitter-textobjects-legacy'

require('nvim-treesitter.configs').setup {
  highlight = { enable = true },
  indent = { enable = true },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['a='] = { query = '@assignment.outer', desc = '@assignment.outer' },
        ['i='] = { query = '@assignment.inner', desc = '@assignment.inner' },
        ['[='] = { query = '@assignment.lhs', desc = '@assignment.lhs' },
        [']='] = { query = '@assignment.rhs', desc = '@assignment.rhs' },
        -- vim use m for methods e.g. [m
        ['am'] = { query = '@function.outer', desc = '@function.outer' },
        ['im'] = { query = '@function.inner', desc = '@function.inner' },
        ['ac'] = { query = '@call.outer', desc = '@call.outer' },
        ['ic'] = { query = '@call.inner', desc = '@call.inner' },
        -- not selecting multiple lines of comment - FIXME
        -- ic = { query = '@comment.inner', desc = '@comment.inner' },
        -- ac = { query = '@comment.outer', desc = '@comment.outer' },
        ['aC'] = { query = '@class.outer', desc = '@class.outer' },
        ['iC'] = { query = '@class.inner', desc = '@class.inner' },
        -- a taken by <
        ['aA'] = { query = '@parameter.outer', desc = '@parameter.outer' },
        ['iA'] = { query = '@parameter.inner', desc = '@parameter.inner' },
        ['as'] = {
          query = '@local.scope',
          query_group = 'locals',
          desc = '@local.scope',
        },
        ['at'] = { query = '@type' },
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['g]A'] = '@parameter.inner',
        ['g]m'] = '@function.outer',
      },
      swap_previous = {
        ['g[A'] = '@parameter.inner',
        ['g[m'] = '@function.outer',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
        [']o'] = '@loop.*',
        [']/'] = '@comment.outer',
        [']*'] = '@comment.outer',

        -- You can pass a query group to use query from `queries/<lang>/<query_group>.scm file in your runtime path.
        -- Below example nvim-treesitter's `locals.scm` and `folds.scm`. They also provide highlights.scm and indent.scm.
        [']s'] = {
          query = '@local.scope',
          query_group = 'locals',
          desc = 'Next scope',
        },
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
        ['[/'] = '@comment.outer',
        ['[*'] = '@comment.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
      -- go to either the start or the end, whichever is closer
      goto_next = {},
      goto_previous = {},
    },
    lsp_interop = {
      enable = true,
      floating_preview_opts = { border = 'none' },
      -- show textobject surrounding definition as determined using Neovim's
      -- built-in LSP in a floating window.
      peek_definition_code = {
        ['<leader>df'] = '@function.outer',
        ['<leader>dF'] = '@class.outer',
      },
    },
  },
}

local ts_repeat = require 'nvim-treesitter.textobjects.repeatable_move'

vim.keymap.set({ 'n', 'x', 'o' }, ';', ts_repeat.repeat_last_move)
vim.keymap.set(
  { 'n', 'x', 'o' },
  'f',
  ts_repeat.builtin_f_expr,
  { expr = true }
)
vim.keymap.set(
  { 'n', 'x', 'o' },
  'F',
  ts_repeat.builtin_F_expr,
  { expr = true }
)
vim.keymap.set(
  { 'n', 'x', 'o' },
  't',
  ts_repeat.builtin_t_expr,
  { expr = true }
)
vim.keymap.set(
  { 'n', 'x', 'o' },
  'T',
  ts_repeat.builtin_T_expr,
  { expr = true }
)

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
