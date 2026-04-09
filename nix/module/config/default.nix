{
  config,
  lib,
  wlib,
  ...
}:
{
  imports = [
    ./specs
    ./settings
  ];

  config.specMods =
    { ... }:
    {
      options.extraPackages = lib.mkOption {
        type = lib.types.listOf wlib.types.stringable;
        default = [ ];
        description = "a extraPackages spec field to put packages to suffix to the PATH";
      };
    };

  config.extraPackages = config.specCollect (
    acc: v: acc ++ (v.extraPackages or [ ])
  ) [ ];
}
