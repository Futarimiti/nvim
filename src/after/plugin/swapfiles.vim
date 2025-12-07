" :Swap same as :swapname
" :Swap! to delete swapfile of current buffer
command -bang Swap call s:swap(<bang>0)

function s:swap(bang) abort
  if a:bang
    let swapfile = bufnr()->swapname()
    call jobstart(['trash', swapfile])
    echo $'!trash {swapfile}'
  else
    swapname
  endif
endfunction
