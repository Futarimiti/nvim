```bash
rm -rf ~/.config/nvim ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim
brew tap austinliuigi/brew-neovim-nightly https://github.com/austinliuigi/brew-neovim-nightly.git
brew install neovim-nightly
git clone --recurse-submodules -b main git@github.com:Futarimiti/nvim.git ~/.config/nvim
```

- [x] General options, keymaps, autocmds, packages
- [x] Quick fuzzy find files in current dir, in user config dir, and in `$VIMRUNTIME`
- [ ] Set up comment, indent, formatters, compilers, include-search, fold for each ft
    - [x] Get the nasty sql thing done
- [ ] Set up LSP for some ft only if desperate
- [x] `gO`-like outline navigation for LaTeX, markdown etc
- [ ] Way to 'refresh' vim after changing config without leaving editor
