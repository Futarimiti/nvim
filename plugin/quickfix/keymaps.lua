local qf = function()
  if vim.iter(vim.fn.getwininfo()):any(function(win) return win.quickfix == 1 end) then
    vim.cmd.cclose()
  else
    vim.cmd.cwindow()
  end
end
vim.keymap.set('n', '<localleader>q', qf)
vim.keymap.set('n', '<localleader>c', qf)
vim.keymap.set('n', '<localleader>l', function()
  if vim.iter(vim.fn.getwininfo()):any(function(win) return win.loclist == 1 end) then
    vim.cmd.lclose()
  else
    vim.cmd.lwindow()
  end
end)
