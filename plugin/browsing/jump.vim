if !executable('autojump')
  finish
endif

function s:j(dest)
  " system() produces a newline at the end
  let [dest] = system(['autojump', a:dest])->split()
  call system(['autojump', '--add', dest])
  lcd `=dest`
  pwd
endfunction

function s:completion(A,L,P)
  return system(['autojump', '--complete', a:A])
        \->split()
        \->map({_, s -> substitute(s, '^.*__\d__', '', '')})
        \->uniq()
endfunction

command -complete=customlist,s:completion -nargs=1 J call s:j(<f-args>)

augroup dirfootprint
  autocmd!
  " does not count in autochdir - you are unconscious of that
  autocmd DirChanged window,tabpage,global
        \ call system(['autojump', '--add', v:event.cwd])
augroup END
