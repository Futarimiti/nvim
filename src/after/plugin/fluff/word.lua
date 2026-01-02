-- MS Word experience

local ns = vim.api.nvim_create_namespace ''

-- `:Hi <higroup>` applies given highlight group to the last visual selection
-- `:Hi` accepts ranges but never uses them;
-- it's solely for convenience when used in visual mode
vim.api.nvim_create_user_command('Hi', function(args)
  local higroup = args.args
  local start_row, start_col = unpack(vim.fn.getpos '\'<', 2, 3)
  local end_row, end_col = unpack(vim.fn.getpos '\'>', 2, 3)
  vim.hl.range(
    0,
    ns,
    higroup,
    { start_row - 1, start_col - 1 },
    { end_row - 1, end_col - 1 },
    { inclusive = true }
  )
end, {
  complete = 'highlight',
  nargs = 1,
  range = true,
  bar = true,
  desc = 'highlight last charwise or linewise visual selection (blocks are not supported)',
})

-- `:[range]HiLines <higroup>` applies given highlight group to lines
-- covered by the range (default current line)
vim.api.nvim_create_user_command('HiLines', function(args)
  local higroup = args.args
  vim.hl.range(0, ns, higroup, { args.line1 - 1, 0 }, { args.line2, 0 })
end, {
  complete = 'highlight',
  nargs = 1,
  range = true,
  bar = true,
  desc = 'highlight arbitrary lines',
})

vim.api.nvim_create_user_command(
  'ResetLines',
  function(args) vim.api.nvim_buf_clear_namespace(0, ns, args.line1 - 1, args.line2) end,
  {
    bar = true,
    range = true,
    desc = 'clear highlight clusters on current line or in given range',
  }
)
