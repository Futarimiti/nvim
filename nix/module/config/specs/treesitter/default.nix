# we still need nvim-treesitter for indent
# (it's not not in core yet https://github.com/neovim/neovim/issues/38818)
{ pkgs, ... }:
{
  config.specs.treesitter = {
    lazy = true;
    data =
      let
        p = pkgs.vimPlugins;
        parsers = import ./parsers.nix;
      in
      [
        (p.nvim-treesitter.withPlugins parsers)
        p.nvim-treesitter-textobjects
        p.treewalker-nvim
      ];
    extraPackages = [ ];
  };
}
