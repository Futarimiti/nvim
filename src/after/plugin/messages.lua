require('vim._core.ui2').enable { enable = true, msg = { target = 'msg' } }

-- :messages now won't output anything but instead creates a split
-- meaning vim-scriptease :Messages will no longer work; overriding it
-- load scriptease now to make sure :Messages won't be redefined later
vim.cmd.packadd 'vim-scriptease'
vim.api.nvim_create_user_command(
  'Messages',
  function(o) vim.cmd.messages(o.fargs) end,
  { nargs = '?', bar = true }
)

vim.keymap.set({ 'n', 'x' }, '<LocalLeader>m', '<Cmd>messages<CR>')
