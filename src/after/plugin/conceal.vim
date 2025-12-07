function ToggleConceal() abort
  if &l:conceallevel is 3
    setlocal conceallevel=0
  elseif &l:conceallevel is 2
    setlocal conceallevel=0
  else
    setlocal conceallevel=2
  endif
endfunction
command ToggleConceal call ToggleConceal()

function s:space_C() abort
  if v:count is 0
    ToggleConceal
  else
    let &l:conceallevel = v:count
  endif
endfunction

nnoremap <LocalLeader>C <Cmd>call <SID>space_C()<CR>
