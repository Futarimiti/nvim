function! s:browse_oldfiles() abort
  function! OldFiles(arg, y, z) abort
    return v:oldfiles->copy()->filter({ _, f -> f =~? a:arg })
  endfunction
  try
    let file = input('Browse oldfiles: ', '', 'customlist,OldFiles')
    if file !~ '^\s*$'
      edit `=file`
    endif
  finally
    delfunction! OldFiles
  endtry
endfunction
nnoremap <localleader>o <CMD>call <SID>browse_oldfiles()<CR>
nnoremap <localleader>O :vimgrep  `=v:oldfiles`<C-Left><Left>
