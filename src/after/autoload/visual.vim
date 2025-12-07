function visual#selection() abort
  return getregion(getpos('v'), getpos('.'), #{type: mode()})
endfunction
