---@type vim.lsp.Config
return {
  name = 'luals',
  filetypes = { 'lua' },
  cmd = { 'lua-language-server' },
  root_markers = { '.luarc.json', '.luarc.jsonc' },
}
