set completeopt=menuone,noselect

" simple keyword autocomplete trigger by 'iskeyword' characters

function! s:maybe_complete()
  if !exists('g:autocomplete_in_progress')
        \&& !pumvisible()
        \&& state('m') == ''
        \&& &buftype == ''
        \&& v:char =~# '\k'
    let g:autocomplete_in_progress = 1
    call feedkeys("\<C-N>", 'ni')
  endif
endfunction

augroup simple-keyword-autocomplete
  autocmd!
  autocmd InsertCharPre * call <SID>maybe_complete()
  autocmd TextChangedP,TextChangedI * 
        \if exists('g:autocomplete_in_progress')
        \| unlet g:autocomplete_in_progress
        \| endif
augroup END
