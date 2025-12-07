{ inputs, pkgs, ... }:
let
  parsers =
    ps: with ps; [
      bash
      c
      c_sharp
      comment
      css
      dhall
      editorconfig
      git_config
      git_rebase
      gitattributes
      gitcommit
      gitignore
      go
      groovy
      haskell
      haskell_persistent
      idris
      java
      javascript
      json
      just
      koka
      latex
      lua
      luadoc
      markdown
      nix
      properties
      python
      query
      regex
      rust
      scala
      scheme
      sql
      toml
      typst
      vim
      vimdoc
      xml
      yaml
      zig
    ];
  koka =
    inputs.tree-sitter-koka.packages.${pkgs.stdenv.hostPlatform.system}.default;
  nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.overrideAttrs {
    src = inputs.nvim-treesitter;
  };
in
nvim-treesitter.withPlugins parsers
