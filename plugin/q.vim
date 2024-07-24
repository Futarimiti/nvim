function! s:Q() abort
  if empty(&l:formatprg)
    write
    return 
  endif
  let view = winsaveview()
  let buf = bufnr()
  let original = getbufline(buf, 1, '$')
  execute 'silent %!' .. &l:formatprg
  if v:shell_error
    echohl ErrorMsg
    for errmsg in getbufline(buf, 1, '$')
      echo errmsg
    endfor
    echohl None
    call setbufline(buf, 1, original)
  else
    write
  endif
  call winrestview(view)
endfunction

nnoremap Q <CMD>call <SID>Q()<CR>
