augroup auto-create-parent
  autocmd!
  autocmd BufWritePre,FileWritePre *
        \ if @% !~ '://' | call mkdir(expand('<afile>:p:h'), 'p') | endif
augroup END

augroup auto-cursorline
  autocmd!
  autocmd WinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
