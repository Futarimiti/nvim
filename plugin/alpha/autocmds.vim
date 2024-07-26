augroup alpha
  autocmd!
  autocmd BufWritePre,FileWritePre * if @% !~# '\(://\)' | call mkdir(expand('<afile>:p:h'), 'p') | endif
  autocmd TextYankPost * lua vim.highlight.on_yank { higroup = 'Visual', timeout = 200 }
augroup END
