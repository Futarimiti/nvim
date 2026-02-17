{ inputs, system }:
{ ... }:
{
  settings = {
    suffix-path = true;
    suffix-LD = true;
    wrapRc = true;
    aliases = [ ];
    neovim-unwrapped = inputs.neovim-nightly-overlay.packages.${system}.neovim;
  };
  categories = {
    general = true;
    test = true;
  };
}
