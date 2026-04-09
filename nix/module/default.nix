inputs:
{ wlib, ... }:
{
  imports = [
    wlib.wrapperModules.neovim
    ./config
  ];

  _module.args = { inherit inputs; };
}
