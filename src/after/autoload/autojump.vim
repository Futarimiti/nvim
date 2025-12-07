function autojump#j(dest) abort
  if !executable('autojump')
    echoerr 'cannot find autojump executable'
    return
  endif
  let res = systemlist(['autojump', a:dest])
  if len(res) is 1
    let [dest] = res
    " use cd for global
    lcd `=dest`
    pwd
  else
    echoerr 'unexpected autojump output: ' .. string(res)
    return
  endif
endfunction

function autojump#completion(A,L,P) abort
  return systemlist(['autojump', '--complete', a:A])
        \->map({ _, s -> substitute(s, '^.*__\d__', '', '') })
        \->uniq()
endfunction

