{
  config,
  inputs,
  lib,
  pkgs,
  wlib,
  ...
}:
{
  imports = [
    ./specs
    ./settings
  ];

  config = {
    package =
      inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.neovim;

    specMods =
      { ... }:
      {
        options.extraPackages = lib.mkOption {
          type = lib.types.listOf wlib.types.stringable;
          default = [ ];
          description = "a extraPackages spec field to put packages to suffix to the PATH";
        };
      };

    extraPackages = config.specCollect (
      acc: v: acc ++ (v.extraPackages or [ ])
    ) [ ];
  };
}
