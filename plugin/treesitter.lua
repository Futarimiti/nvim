vim.keymap.set('n', '<localleader>I', vim.treesitter.inspect_tree)

require('nvim-treesitter.configs').setup {
  ensure_installed = { 'haskell' },
  highlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = {},
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  indent = {
    enable = true,
  },
}
