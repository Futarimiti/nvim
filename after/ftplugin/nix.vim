if expand('%:p') is? glob('~/.config/nix-darwin/flake.nix')
  " may need touch ID
  let b:dispatch = 'darwin-rebuild switch --flake %:h'
endif

setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
