{ pkgs, ... }:
{
  config.specs = {
    startup = {
      data = with pkgs.vimPlugins; [
        YankAssassin-vim
        nvim-parinfer
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

    optional = {
      lazy = true;
      data = with pkgs.vimPlugins; [
        bufjump-nvim
        bullets-vim
        camelcasemotion
        cmdalias-vim
        stringbreaker-nvim
        switch-vim
        term-edit-nvim
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
        vimade
      ];
    };
  };
}
