set completeopt=menuone,noselect

" simple filepath & keyword autocomplete
" b:autocomplete_extra_isk: extra isk keys to be used during autocompletion.
" Format same as 'iskeyword' option. Will be appended to local 'iskeyword'
" temporarily during completion, then restored after complete done.
" b:autocomplete_key: which key to be fed. Default <C-N>.

function! s:maybe_complete()
  if exists('g:autocomplete_in_progress')
        \ || pumvisible() || state('m') == 'm' || &buftype != ''
    return
  endif

  let g:autocomplete_in_progress = 1
  if v:char == '/'
    call feedkeys("\<C-X>\<C-F>", 'ni')
    return
  endif

  if exists('b:autocomplete_extra_isk')
    let g:isk_before_autocomplete = &l:iskeyword
    let &l:iskeyword .= ',' .. b:autocomplete_extra_isk
  endif

  if v:char =~# '\k'
    let key = get(b:, 'autocomplete_key', "\<C-N>")
    call feedkeys(key, 'ni')
  endif
endfunction

function! s:restore()
  if exists('g:autocomplete_in_progress')
    unlet g:autocomplete_in_progress
  endif
  if exists('g:isk_before_autocomplete')
    let &l:iskeyword = g:isk_before_autocomplete
    unlet g:isk_before_autocomplete
  endif
endfunction

augroup simple-autocomplete
  autocmd!
  autocmd InsertCharPre * call <SID>maybe_complete()
  autocmd TextChangedP,TextChangedI * call <SID>restore()
augroup END
