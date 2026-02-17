{ inputs, system, ... }:
{
  nvim = import ./nvim.nix { inherit inputs system; };
}
