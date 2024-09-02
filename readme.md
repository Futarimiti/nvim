```bash
# get these ready: git j tree-sitter nvim (nightly)
rm -rf ~/.config/nvim ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim
mkdir ~/.cache/nvim/  # nvim-hs somehow stumble upon this
git clone --recurse-submodules -b nightly git@github.com:Futarimiti/nvim.git ~/.config/nvim
nvim --cmd 'helptags ALL'
```

- [x] General options, keymaps, autocmds, packages
- [x] Quick fuzzy find files in current dir, in user config dir, and in `$VIMRUNTIME`
- [x] Get the nasty sql ftplugin thing done
- [x] `gO`-like outline navigation for LaTeX, markdown etc
- [ ] Way to 'refresh' vim after changing config without leaving editor

## Long-term goals

- Set up comment, indent, formatters, compilers, include-search, fold for each ft
- Set up TS/LSP for some ft only if desperate
