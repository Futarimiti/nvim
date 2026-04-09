{ pkgs, ... }:
{
  config.specs.file-explorers = {
    lazy = true;
    data = with pkgs.vimPlugins; [
      vim-vinegar
    ];
    extraPackages = with pkgs; [
      darwin.trash # netrw
    ];
  };
}
