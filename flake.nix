{
  description = "My neovim flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=62b21fb4436e32c7884191bc2fcbb4dc2726f160";
    nixCats.url = "github:BirdeeHub/nixCats-nvim";
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    json-fmt = {
      url = "github:Futarimiti/json-fmt/v3-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tree-sitter-koka = {
      url = "github:Futarimiti/tree-sitter-koka/fix/flake-overlay-src";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tree-sitter-haskell = {
      url = "github:tree-sitter/tree-sitter-haskell";
      flake = false;
    };
    nvim-treesitter = {
      url = "github:nvim-treesitter/nvim-treesitter/main";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      nixCats,
      neovim-nightly-overlay,
      json-fmt,
      ...
    }@inputs:
    nixCats.utils.eachSystem nixpkgs.lib.platforms.all (
      system:
      let
        inherit (nixCats) utils;
        extra_pkg_config = { };
        dependencyOverlays = [ ];
        categories =
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
                nixfmt-rfc-style
                python312Packages.autopep8
                python312Packages.sqlparse # sqlformat
                json-fmt.packages.${system}.json-fmt
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
                nvim-treesitter-textobjects
                otter-nvim
                quicker-nvim
                stringbreaker-nvim
                switch-vim
                term-edit-nvim
                (import ./nix/treesitter { inherit inputs pkgs; })
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
          };
        package = {
          nvim =
            { ... }:
            {
              settings = {
                suffix-path = true;
                suffix-LD = true;
                wrapRc = true;
                aliases = [ ];
                neovim-unwrapped = neovim-nightly-overlay.packages.${system}.neovim;
              };
              categories = {
                general = true;
                test = true;
              };
            };
        };
        defaultPackageName = "nvim";
        defaultPackage = utils.baseBuilder ./src {
          inherit
            nixpkgs
            system
            dependencyOverlays
            extra_pkg_config
            ;
        } categories package defaultPackageName;
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages = utils.mkAllWithDefault defaultPackage;
        devShells = {
          default = pkgs.mkShell {
            name = defaultPackageName;
            packages = [ defaultPackage ];
            inputsFrom = [ ];
          };
        };
      }
    );
}
