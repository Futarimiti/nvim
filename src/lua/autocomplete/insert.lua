-- module: insert mode autocompletion
local M = {}

-- augroup id for the autocomplete autocmds
M.group = vim.api.nvim_create_augroup('ins-autocomplete', {})

-- given 'completion' = bringing up the completion popup menu:
-- when typing too quickly and/or vim scanning too many words for completion
-- the completion trigger may fire again before previous completion has finished
-- resulting in double-feeds of <C-N> or whatever which is bad
-- so we need this (ugly) lock variable to track if there's already a completion
-- in progress and kill any attempts to trigger another completion if yes
-- a timer should be a less coarse solution but it works so touch it not
M.complete_in_progress = false

local feed = function(keys)
  vim.api.nvim_feedkeys(vim.keycode(keys), 'ni', false)
end

M.enable = function()
  vim.api.nvim_create_autocmd('InsertCharPre', {
    desc = 'filepath & omni & keyword completion',
    group = M.group,
    callback = function(args)
      if
        M.complete_in_progress
        or vim.fn.pumvisible() ~= 0 -- visible
        or vim.tbl_contains(
          { 'terminal', 'prompt', 'help' },
          vim.bo[args.buf].buftype
        )
      then
        return
      end

      M.complete_in_progress = true -- lock

      if vim.v.char == '/' then
        feed '<C-X><C-F>'
      elseif
        vim.bo[args.buf].omnifunc ~= '' and vim.v.char:match '%s' == nil
      then
        feed '<C-X><C-O>'
      elseif
        vim.fn.match(vim.v.char, [[\k]]) ~= -1 -- inserted keyword
      then
        feed '<C-N>'
      end
    end,
  })

  -- reset the lock upon text change or insert leave.
  -- (with our approach) the input character won't be displayed
  -- (e.g. TextChanged events won't be trigger) until candidates are found
  -- therefore we can confidently say completion has finished
  -- and pum has shown up upon text change
  vim.api.nvim_create_autocmd(
    { 'TextChangedP', 'TextChangedI', 'InsertLeave' },
    {
      desc = 'reset complete_in_progress lock',
      group = M.group,
      callback = function() M.complete_in_progress = false end,
    }
  )
end

M.disable = function()
  vim.api.nvim_clear_autocmds { group = M.group }
  M.complete_in_progress = false
end

return M
