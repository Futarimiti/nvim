require('vim._extui').enable { enable = true, msg = { target = 'msg' } }

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

-- toggle :messages
-- vim.keymap.set('n', '<LocalLeader>m', function()
--   local current_win = vim.api.nvim_get_current_win()
--   if vim.w[current_win].i_am_messages then
--     vim.api.nvim_win_close(current_win, false)
--   else
--     vim.cmd.messages()
--     if vim.api.nvim_get_current_win() == current_win then
--       -- this is when no :messages split has been created
--       return
--     else
--       vim.w.i_am_messages = true
--     end
--   end
-- end)
--
-- vim.keymap.set({ 'n', 'x' }, 'g<', function()
--   local current_win = vim.api.nvim_get_current_win()
--   if vim.w[current_win].i_am_messages then
--     vim.api.nvim_win_close(current_win, false)
--   else
--     vim.api.nvim_feedkeys('g<', 'ni', false)
--     if vim.api.nvim_get_current_win() == current_win then
--       -- this is when no :messages split has been created
--       return
--     else
--       vim.w.i_am_messages = true
--     end
--   end
-- end)
