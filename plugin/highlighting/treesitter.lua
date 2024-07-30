require('nvim-treesitter.configs').setup {
  ensure_installed = { 'haskell' },
  highlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = {},
  },
  incremental_selection = {
    enable = true,
  },
  indent = {
    enable = true,
  },
}
