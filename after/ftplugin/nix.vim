if expand('%:p:h') is? glob('~/.config/nix-darwin')
  " may need touch ID
  let b:dispatch = 'darwin-rebuild switch --flake ~/.config/nix-darwin --show-trace'
endif

setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
setlocal iskeyword+=-
