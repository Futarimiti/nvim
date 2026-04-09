{
  description = "My neovim flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    wrappers.url = "github:BirdeeHub/nix-wrapper-modules";
    json-fmt = {
      url = "github:Futarimiti/json-fmt/v3-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    let
      module = nixpkgs.lib.modules.importApply ./nix/module inputs;
      wrapper = wrappers.lib.evalModule module;
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { system, ... }:
        {
          packages.default =
            let
              pkgs = import nixpkgs { inherit system; };
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
