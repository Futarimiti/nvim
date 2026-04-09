{ pkgs, ... }:
let
  plugins = pkgs.vimPlugins;
in
{
  imports = [
    ./misc
    ./lsps.nix
    ./formatters.nix
    ./runtime-deps.nix
    ./file-explorers.nix
    ./treesitter
  ];

  config.specs = {
    view = plugins.restore-view-vim;

    qf = {
      data = plugins.quicker-nvim;
      lazy = true;
    };

    db = {
      data = with plugins; [
        vim-dadbod
        vim-dadbod-completion
        vim-dadbod-ui
      ];
      lazy = true;
    };

    text-object = {
      data = with plugins; [
        vim-textobj-comment
        vim-textobj-entire
        vim-textobj-function
        vim-textobj-user
      ];
      lazy = true;
    };
  };
}
