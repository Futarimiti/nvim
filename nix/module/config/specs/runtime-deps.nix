{ pkgs, ... }:
{
  config.specs.runtime-deps = {
    data = null;
    extraPackages = with pkgs; [
      autojump # :J
      ripgrep # :grep
    ];
  };
}
