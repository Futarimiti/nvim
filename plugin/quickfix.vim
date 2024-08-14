nnoremap [q <CMD>silent cprevious<CR>
nnoremap ]q <CMD>silent cnext<CR>
nnoremap [c <CMD>silent cprevious<CR>
nnoremap ]c <CMD>silent cnext<CR>
nnoremap [l <CMD>silent lprevious<CR>
nnoremap ]l <CMD>silent lnext<CR>

function! s:toggle_loclist() abort
  if getwininfo()->filter({ _, e -> e.loclist })->empty()
    silent! lopen
  else
    lclose
  endif
endfunction

function! s:toggle_quickfix() abort
  if getwininfo()->filter({ _, e -> e.quickfix })->empty()
    copen
  else
    cclose
  endif
endfunction

nnoremap <localleader>q <CMD>call <SID>toggle_quickfix()<CR>
nnoremap <localleader>c <CMD>call <SID>toggle_quickfix()<CR>
nnoremap <localleader>l <CMD>call <SID>toggle_loclist()<CR>

packadd cfilter
Alias cfilter Cfilter
Alias lfilter Lfilter
