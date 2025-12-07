---@type vim.lsp.Config
return {
  name = 'basedpyright-no-diagnostics',
  filetypes = { 'python' },
  cmd = { 'basedpyright-langserver', '--stdio' },
  root_markers = { '__init__.py', 'pyrightconfig.json' },
  handlers = {
    [vim.lsp.protocol.Methods.textDocument_publishDiagnostics] = function(_, _, _) end,
  },
}
