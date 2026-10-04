---@type vim.lsp.Config
return {
  name = 'nixd',
  cmd = { 'nixd' },
  filetypes = { 'nix' },
  root_markers = { 'flake.nix' },
  settings = {
    nixd = {
      nixpkgs = {
        expr = 'import <nixpkgs> { }',
      },
      diagnostic = { suppress = { 'sema-primop-overridden' } },
    },
  },
}
