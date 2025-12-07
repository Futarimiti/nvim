" https://www.reddit.com/r/vim/comments/1l35p39/exclude_from_isfname
set isfname+=^194

augroup AutoCreateParent
  autocmd!
  autocmd BufWritePre,FileWritePre *
        \ if @% !~ '://' | call mkdir(expand('<afile>:p:h'), 'p') | endif
augroup END
