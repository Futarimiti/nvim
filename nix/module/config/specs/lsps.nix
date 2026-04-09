{ pkgs, ... }:
{
  config.specs.lsps = {
    data = null;
    extraPackages = with pkgs; [
      lua-language-server
      texlab
      ruff
      basedpyright
      nixd
    ];
  };
}
