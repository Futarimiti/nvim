if findfile('flake.nix', '.;') isnot ''
  let b:dispatch = 'nix build'
  let b:start = 'nix run'
  compiler nix
" elseif expand('%:p:h')->stridx(expand('~/.config/nix-darwin')) is 0
"   " will ask for passwd; recommend set up touch ID
"   let b:dispatch = 'sudo darwin-rebuild switch --flake ~/.config/nix-darwin'
"   let b:start = 'nix repl --expr "builtins.getFlake \"$PWD\""'
"   " often needs sudo, probably not useful
"   compiler darwin-rebuild
else
  let b:dispatch = 'nix-build'
  let b:start = 'nix repl'
  compiler nix-build
endif
