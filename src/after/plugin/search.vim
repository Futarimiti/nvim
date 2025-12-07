" * intentionally ignore wildignores - use external grepprg
" * only search in files with the exact extension (e.g. hs /= lhs, js /= ts)

nnoremap <silent> <nowait> gr <Cmd>call <SID>gr()<CR>
xnoremap <silent> <nowait> gr <Cmd>call <SID>vgr()<CR>

function s:gr() abort
  silent grep! '''\<<cword>\>''' **/*.%:e
  copen
endfunction

function s:vgr() abort
  let sel = visual#selection()->join("\n")
  execute $'silent grep! ''{sel}'' **/*.%:e'
  copen
endfunction
