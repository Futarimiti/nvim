---@type vim.lsp.Config
return {
  name = 'basedpyright',
  filetypes = { 'python' },
  cmd = { 'basedpyright-langserver', '--stdio' },
  root_markers = { '__init__.py', 'pyrightconfig.json' },
}
