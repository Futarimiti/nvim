{
  pkgs,
  inputs,
  ...
}:
{
  config.specs.formatters = {
    data = null;
    extraPackages = with pkgs; [
      stylish-haskell
      haskellPackages.cabal-fmt
      stylua
      astyle
      yamlfmt
      rustfmt
      nixfmt
      python312Packages.autopep8
      python312Packages.sqlparse # sqlformat
      inputs.json-fmt.packages.${pkgs.stdenv.system}.json-fmt
      typstyle
      js-beautify
    ];
  };
}
