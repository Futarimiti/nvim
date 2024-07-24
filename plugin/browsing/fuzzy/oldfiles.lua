vim.api.nvim_create_user_command('BrowseOldfiles', function(o) vim.cmd.edit(o.args) end, {
  nargs = 1,
  complete = function(arg, _, _)
    return vim
      .iter(vim.v.oldfiles)
      :filter(function(f) return vim.re.find(f, vim.glob.to_lpeg(arg)) ~= nil end)
      :totable()
  end,
  desc = 'fuzzy search in oldfiles',
})

vim.keymap.set('n', '<localleader>o', ':BrowseOldfiles ')
vim.keymap.set('n', '<localleader>O', ':vimgrep  `=v:oldfiles`<C-Left><Left>')
