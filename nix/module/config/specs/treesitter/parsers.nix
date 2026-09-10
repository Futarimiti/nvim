{ pkgs, ... }:
ps:
[
  pkgs.vimPlugins.nvim-treesitter.grammarPlugins.atob
  (pkgs.vimPlugins.nvim-treesitter.grammarToPlugin pkgs.tree-sitter-grammars.tree-sitter-mail)
]
++ (
  with ps; # pkgs.tree-sitter.builtGrammars.tree-sitter-*
  [
    # atob # undefined variable
    bash
    c
    c_sharp
    comment
    css
    dhall
    diff
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
    html
    htmldjango
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
