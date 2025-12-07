-- BETA
if true then return end

vim.cmd [[set wim=noselect:lastused,full wop=pum wcm=<C-@> wmnu]]

-- works with cmdline pum as well - tested
local pumvisible = function() return vim.fn.pumvisible() ~= 0 end

local cmd_complete = function(cur_cmdline)
  local cmdline = vim.fn.getcmdline()
  local curpos = vim.fn.getcmdpos()
  if
    cur_cmdline == cmdline
    and not pumvisible()
    and curpos == #cmdline + 1
    and vim.fn.match(cmdline:sub(curpos - 2, curpos - 2), [=[[\w*/:]]=])
  then
    vim.api.nvim_feedkeys(vim.keycode '<C-@>', 'ti', false)
    vim.opt.eventignore:append 'CmdlineChanged'
    vim.fn.timer_start(0, function(_)
      vim.fn.setcmdline(vim.fn.substitute(vim.fn.getcmdline(), [[\%x00$]], '', ''))
      vim.opt.eventignore:remove 'CmdlineChanged'
    end)
  end
end

vim.api.nvim_create_autocmd('CmdlineChanged', {
  desc = 'cmdline autocomplete',
  group = vim.api.nvim_create_augroup('cmdline-autocomplete', {}),
  pattern = ':',
  callback = function()
    vim.fn.timer_start(50, function(_) cmd_complete(vim.fn.getcmdline()) end)
  end,
})
