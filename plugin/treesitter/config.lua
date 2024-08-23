require('nvim-treesitter.configs').setup {
  ensure_installed = {
    'comment',
    'scala',
    'latex',
    -- broken shipped parsers
    'bash',
    'query',
    'vimdoc',
    -- haskell & injections (see queries/haskell/injections.scm)
    'haskell',
    'css',
    'html',
    'javascript',
    'typescript',
    'json',
    'sql',
    'haskell_persistent',
  },
  highlight = {
    enable = true,
    disable = {},
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      -- below are vmap
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  indent = {
    enable = true,
  },
}
