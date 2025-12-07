---@type vim.lsp.Config
return {
  name = 'tinymist',
  filetypes = { 'typst' },
  cmd = { 'tinymist' },
  root_markers = { '.git' },
  -- semantic highlights not working properly with CJK characters
  -- disabled for now
  on_attach = function(client)
    client.server_capabilities.semanticTokensProvider = nil
  end,
}
