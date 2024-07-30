require('nvim-treesitter.configs').setup {
  ensure_installed = {},
  highlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = { 'haskell' },
  },
}
