---@type vim.lsp.Config
return {
  name = 'ruff',
  filetypes = { 'python' },
  cmd = { 'ruff', 'server' },
  root_markers = { 'pyproject.toml', 'ruff.toml', '.ruff.toml' },
}
