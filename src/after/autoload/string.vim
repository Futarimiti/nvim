function string#capitalise(s) abort
  if empty(a:s)
    return ''
  endif
  return toupper(a:s[0]) .. a:s[1:]
endfunction
