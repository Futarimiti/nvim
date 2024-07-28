set completeopt=menuone,noselect

" simple keyword autocomplete
" trigger char pattern customisable, by default '[a-z0-9._]'

function! s:maybe_complete()
  if pumvisible() || state('m') == 'm'
    return
  endif
  if v:char =~? get(b:, 'autocomplete_pattern', '[a-z0-9._]')
    call feedkeys("\<C-N>", 'ni')
  endif
endfunction

augroup simple-keyword-autocomplete
  autocmd!
  autocmd InsertCharPre * call <SID>maybe_complete()
augroup END
