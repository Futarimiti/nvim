{ inputs, pkgs, ... }:
let
  parsers =
    ps:
    (with ps; [
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
      haskell_persistent
      idris
      java
      javascript
      json
      just
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
    ])
    ++ [ haskell ];
  haskell = pkgs.tree-sitter.buildGrammar {
    language = "haskell";
    version = inputs.tree-sitter-haskell.rev;
    src = inputs.tree-sitter-haskell;
  };
in
{
  config.specs.treesitter = {
    lazy = true;
    data = with pkgs.vimPlugins; [
      (nvim-treesitter-legacy.withPlugins parsers)
      nvim-treesitter-textobjects-legacy
      treewalker-nvim
    ];
    extraPackages = [ pkgs.tree-sitter ];
  };
}
