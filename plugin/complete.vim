set completeopt=menuone,noselect

" simple keyword autocomplete
" trigger char pattern customisable, by default '[a-z0-9._]'

function! s:maybe_complete()
  if exists('g:autocomplete_in_progress')
        \|| pumvisible()
        \|| state('m') == 'm'
        \|| &buftype != ''
    return
  endif
  if v:char =~? get(b:, 'autocomplete_pattern', '[a-z0-9._]')
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
