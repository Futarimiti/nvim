-- must set before vim-vinegar got sourced and starts to pick up wig
vim.opt.wildignore:append {
  '*.o',
  '*.obj',
  '*.dyn_hi',
  '*.dyn_o',
  '**/dist-newstyle/**',
  '*.class',
  '*.ibc',
  '*.pyc',
  '__pycache__/',
  'node_modules/',
  '*.a',
  '*.hi',
  '.DS_Store',
  '**/.git/**',
  '**/.direnv/**',
}

vim.cmd.packadd 'vim-vinegar'
