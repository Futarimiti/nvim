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

-- limitations:
-- some patterns are not recognised verbatim
vim.keymap.set('x', 'gr', function()
  local selection = vim.fn.join(
    vim.fn.getregion(vim.fn.getpos 'v', vim.fn.getpos '.', { type = vim.fn.mode() }),
    '\n'
  )
  vim.cmd.vimgrep {
    ('/%s/'):format(selection),
    '**/*.%:e',
    mods = { silent = true },
    bang = true,
  }
  vim.cmd.copen()
end, { silent = true, nowait = true })
