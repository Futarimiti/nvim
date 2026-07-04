{ pkgs, ... }:
{
  config.specs.file-explorers = {
    lazy = true;
    data = with pkgs.vimPlugins; [
      oil-nvim
    ];
    extraPackages = with pkgs; [
      darwin.trash
    ];
  };
}
