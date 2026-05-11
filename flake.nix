{
  description = "My neovim flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    wrappers.url = "github:BirdeeHub/nix-wrapper-modules";
    json-fmt.url = "github:Futarimiti/json-fmt/v3-nix";
    tree-sitter-haskell = {
      url = "github:tree-sitter/tree-sitter-haskell";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      wrappers,
      flake-parts,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { system, ... }:
        {
          packages.default =
            let
              module = nixpkgs.lib.modules.importApply ./nix/module inputs;
              wrapper = wrappers.lib.evalModule module;
              pkgs = import nixpkgs {
                inherit system;
                # vimPlugins that lack license are automatically marked unfree
                # (even when they are actually not)
                # must allow unfree for successful evaluation
                config.allowUnfreePredicate = pkg: pkg.passthru.vimPlugin;
              };
            in
            wrapper.config.wrap { inherit pkgs; };
        };
      systems = [
        # "x86_64-linux"
        # "aarch64-linux"
        # "x86_64-darwin"
        "aarch64-darwin"
      ];
    };

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
