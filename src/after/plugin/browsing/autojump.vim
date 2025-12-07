command -complete=customlist,autojump#completion -nargs=1 J
      \ call autojump#j(<f-args>)

augroup dirfootprint
  autocmd!
  " excluding autochdir (users unaware of that)
  autocmd DirChanged window,tabpage,global
        \ call system(['autojump', '--add', v:event.cwd])
augroup END
