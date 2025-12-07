---@type vim.lsp.Config
return {
  name = 'hls',
  cmd = { 'haskell-language-server-wrapper', '--lsp' },
  filetypes = { 'haskell', 'lhaskell' },
  -- do wildcards work?
  root_markers = { 'stack.yaml', 'package.yaml', '*.cabal', 'cabal.project' },
}
