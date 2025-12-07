function afplay#play(file) abort
  let exe = exepath('afplay')
  if empty(exe)
    echerr 'cannot find afplay executable'
    return
  endif
  call system([exe, a:file])
endfunction
