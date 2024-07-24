```bash
rm -rf ~/.config/nvim ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim
git clone --recurse-submodules -b main git@github.com:Futarimiti/nvim.git ~/.config/nvim
```

- [x] General options, keymaps, autocmds, packages
- [ ] Quick fuzzy find files in current dir, in user config dir, and in `$VIMRUNTIME`
- [ ] Set up comment, indent, formatters, compilers, include-search, fold for each ft
- [ ] Set up LSP for some ft only if desperate
- [ ] `gO`-like outline navigation for LaTeX, markdown etc
- [ ] Way to 'refresh' vim after changing config without leaving editor
