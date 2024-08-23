-- limitations:
-- does not respect wildignore (use external grepprg)
-- only search in files with the exact extension (e.g. hs /= lhs, js /= ts)
vim.keymap.set('n', 'gr', function()
  vim.cmd.grep {
    [['\<<cword>\>']],
    '**/*.%:e',
    mods = { silent = true },
    bang = true,
  }
  vim.cmd.copen()
end, { silent = true, nowait = true })
