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
      options = {
        darwin = {
          expr = '(builtins.getFlake (toString ~/.config/nix-darwin)).darwinConfigurations.Carmans-MacBook-Air.options',
        },
        home_manager = {
          expr = '(builtins.getFlake (toString ~/.config/nix-darwin)).darwinConfigurations.Carmans-MacBook-Air.options.home-manager.users.type.getSubOptions []',
        },
      },
    },
  },
}
