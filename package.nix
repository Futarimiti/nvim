{
  inputs,
  name ? "nvim",
  nixCats ? inputs.nixCats,
  nixpkgs ? inputs.nixpkgs,
  system,
  dependencyOverlays ? [ ],
  extra_pkg_config ? { },
  categories ? import ./nix/categories { inherit inputs system; },
  package ? import ./nix/package { inherit inputs system; },
  pkgsParams ? {
    inherit
      nixpkgs
      system
      dependencyOverlays
      extra_pkg_config
      ;
  },
  ...
}:
nixCats.utils.baseBuilder ./src pkgsParams categories package name
