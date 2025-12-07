vim.keymap.set(
  { 'n', 'v' },
  '<Leader>x',
  [[:substitute/\v(\s*)- \[ \]/\1- \[x\]<CR><CMD>nohlsearch<CR>]],
  { buffer = true, silent = true }
)
