inoremap <expr> <Tab> pumvisible() ? '<C-N>' : '<Tab>'
inoremap <expr> <S-Tab> pumvisible() ? '<C-P>' : '<S-Tab>'
inoremap <expr> <CR> pumvisible() && complete_info().selected isnot -1 ?
      \ '<C-Y>' : '<CR>'
