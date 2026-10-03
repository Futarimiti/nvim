local darwin_options = {
  darwin = {
    expr = '(builtins.getFlake (toString ~/.config/nix-darwin)).darwinConfigurations.Carmans-MacBook-Air.options',
  },
  home_manager = {
    expr = '(builtins.getFlake (toString ~/.config/nix-darwin)).darwinConfigurations.Carmans-MacBook-Air.options.home-manager.users.type.getSubOptions []',
  },
}

local nixos_options = {
  nixos = {
    expr = '(builtins.getFlake (toString ~/.config/nixos)).nixosConfigurations.fhost.options',
  },
  home_manager = {
    expr = '(builtins.getFlake (toString ~/.config/nixos)).nixosConfigurations.fhost.options.home-manager.users.type.getSubOptions []',
  },
}

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
      options = jit.os == 'OSX' and darwin_options or nixos_options,
      diagnostic = {
        suppress = { 'sema-primop-overridden' },
      },
    },
  },
}
