---@type vim.lsp.Config
return {
  name = 'rust-analyzer',
  cmd = { 'rust-analyzer' },
  filetypes = { 'rust' },
  root_markers = { 'Cargo.toml' },
}
