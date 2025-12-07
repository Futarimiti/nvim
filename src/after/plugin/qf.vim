" toggle qf

function Ctoggle(height) abort
  if getwininfo()->filter({ _, win -> win['quickfix'] })->empty()
    Copen
    execute 'copen ' .. a:height
  else
    cclose
  endif
endfunction

command -nargs=? -bar Ctoggle call Ctoggle(<q-args> ?? 10)
nnoremap <expr> <LocalLeader>c $'<Cmd>Ctoggle {v:count ?? 10}<CR>'
nmap <LocalLeader>q <LocalLeader>c

" toggle loclist

function Ltoggle(height) abort
  if getwininfo()->filter({ _, win -> win['loclist'] })->empty()
    try
      execute 'lopen ' .. a:height
    catch
      echohl ErrorMsg
      echo 'E776: No location list'
      echohl None
    endtry
  else
    lclose
  endif
endfunction

command -nargs=? -bar Ltoggle call Ltoggle(<q-args> ?? 10)
nnoremap <expr> <LocalLeader>l $'<Cmd>Ltoggle {v:count ?? 10}<CR>'

" substitute any ^@ with newline
" e.g. nix build
" function QfMakeConv()
"   let qflist = getqflist()
"   for i in qflist
"     let i.text = iconv(i.text, "cp936", "utf-8")
"   endfor
"   call setqflist(qflist)
" endfunction
"
" augroup QFSubstituteNullCharacter
"   autocmd!
"   autocmd QuickfixCmdPost make call QfMakeConv()
" augroup END
