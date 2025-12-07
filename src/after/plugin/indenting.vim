" indenting & formatting

" set shiftwidth=4
" set tabstop=4
" set noexpandtab

function s:indentall() abort
  let view = winsaveview()
  normal! gg=G
  call winrestview(view)
endfunction

" + is <Shift>_=
nnoremap + <Cmd>call <SID>indentall()<CR>

" Script down view in given file
" NB winsaveview() does not remember folds, but :mkview does with vop=folds
function s:writeview(file, viewopt) abort
  let old_vop = &viewoptions
  let &viewoptions = a:viewopt
  mkview! `=a:file`
  let &viewoptions = old_vop
endfunction

function Q() abort
  if empty(&l:formatprg)
    write
    return 
  endif
  let buf = bufnr()
  let original = getbufline(buf, 1, '$')
  " make view of folds & cursor pos of curr win
  " user may have set up BufWritePre mkview, use tempfile to avoid pollution
  let temp = tempname()
  call s:writeview(temp, 'folds,cursor')
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
  " some folds may vanish after formatting which is expected
  " suppress errors for `normal! zo`
  silent! source `=temp`
endfunction
command -bar Q call Q()
nnoremap Q <Cmd>Q<CR>
