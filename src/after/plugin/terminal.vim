tnoremap <S-Space> <Space>

augroup LoadTerminalPackages
  autocmd!
  autocmd TermOpen * ++once packadd term-edit.nvim
        \| lua require('term-edit').setup { prompt_end = '% ' }
augroup END
