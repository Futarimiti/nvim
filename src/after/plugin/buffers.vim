set nohidden

" bdelete all buffers except current one
" named in the same way as :tabonly
command -bang -bar -nargs=0 Bonly silent %bdelete<bang> | edit #

" bdelete all hidden buffers (those not being displayed in any window)
command -bang -bar -nargs=0 Bpurge call buffers#purge(<bang>0)

packadd bufjump.nvim
lua << EOF
local bufjump = require 'bufjump'
bufjump.setup()
vim.keymap.set('n', '<M-o>', bufjump.backward)
vim.keymap.set('n', '<M-i>', bufjump.forward)
EOF
