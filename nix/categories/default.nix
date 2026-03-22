{ inputs, system }:
{ pkgs, ... }:
{
  # available at runtime
  lspsAndRuntimeDeps = {
    general = with pkgs; [
      # lsps
      lua-language-server
      texlab
      ruff
      basedpyright
      nixd

      # formatters
      stylish-haskell
      haskellPackages.cabal-fmt
      stylua
      astyle
      yamlfmt
      rustfmt
      nixfmt
      python312Packages.autopep8
      python312Packages.sqlparse # sqlformat
      inputs.json-fmt.packages.${system}.json-fmt
      typstyle

      # runtime deps
      darwin.trash # netrw
      autojump # :J
      ripgrep # :grep
    ];
  };

  startupPlugins = {
    general = with pkgs.vimPlugins; [
      YankAssassin-vim
      nvim-parinfer
      restore-view-vim
      splitjoin-vim
      tabular
      vim-abolish
      vim-apathy
      vim-characterize
      vim-cool
      vim-dotenv
      vim-endwise
      vim-eunuch
      vim-jdaddy
      vim-ragtag
      vim-repeat
      vim-sexp
      vim-speeddating
      vim-syntax-shakespeare
      whatif-vim
    ];
  };

  # use :packadd
  optionalPlugins = {
    general = with pkgs.vimPlugins; [
      bufjump-nvim
      bullets-vim
      camelcasemotion
      cmdalias-vim
      nvim-treesitter-textobjects-legacy
      otter-nvim
      quicker-nvim
      stringbreaker-nvim
      switch-vim
      term-edit-nvim
      (import ./treesitter { inherit inputs pkgs; })
      treewalker-nvim
      undotree
      vim-dadbod
      vim-dadbod-completion
      vim-dadbod-ui
      vim-dispatch
      vim-fugitive
      vim-obsession
      vim-projectionist
      vim-rhubarb
      vim-scriptease
      vim-sleuth
      vim-slime
      vim-surround
      vim-tbone
      vim-textobj-comment
      vim-textobj-entire
      vim-textobj-function
      vim-textobj-user
      vim-vinegar
      vimade
    ];
  };

  # $LD_LIBRARY_PATH
  sharedLibraries = { };

  environmentVariables = { };

  python3.libraries = { };

  extraLuaPackages = { };
}
