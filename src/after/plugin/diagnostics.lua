vim.diagnostic.config {
  virtual_text = false,
  float = { scope = 'cursor' },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '',
      [vim.diagnostic.severity.WARN] = '',
      [vim.diagnostic.severity.HINT] = '',
      [vim.diagnostic.severity.INFO] = '',
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = 'DiagnosticError',
      [vim.diagnostic.severity.WARN] = 'DiagnosticWarn',
      [vim.diagnostic.severity.HINT] = 'DiagnosticHint',
      [vim.diagnostic.severity.INFO] = 'DiagnosticInfo',
    },
  },
}

-- keymaps
vim.keymap.set(
  'n',
  '[d',
  function() vim.diagnostic.jump { count = -1, float = true } end
)
vim.keymap.set(
  'n',
  ']d',
  function() vim.diagnostic.jump { count = 1, float = true } end
)

vim.keymap.set(
  'n',
  '<LocalLeader>d',
  function()
    vim.diagnostic.enable(not vim.diagnostic.is_enabled(), { bufnr = 0 })
  end,
  { desc = 'toggle diagnostics (buffer local)' }
)

vim.keymap.set(
  'n',
  '<LocalLeader>D',
  function()
    vim.diagnostic.enable(not vim.diagnostic.is_enabled(), { bufnr = nil })
  end,
  { desc = 'toggle diagnostics (all buffers)' }
)
