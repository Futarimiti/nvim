{
  description = "My neovim flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixCats.url = "github:BirdeeHub/nixCats-nvim";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
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
    { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { system, ... }:
        {
          packages.default = import ./package.nix { inherit inputs system; };
        };
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    };
}
