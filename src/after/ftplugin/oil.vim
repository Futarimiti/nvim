" not sure why these seems to pend for another key, nowait it
nnoremap <buffer> <nowait> yy yy
xnoremap <buffer> <nowait> y y

let s:oil = luaeval("require('oil')")
let s:cwd = s:oil.get_current_dir() " always ends in /

cnoremap <buffer> <expr> <C-R><C-F> <SID>line_get_relpath(line('.'))
nnoremap <buffer> . :<C-U><Space><C-R>=<SID>escaped(line('.'),line('.')-1+v:count1)<CR><Home>
xnoremap <buffer> . <Esc>:<Space><C-R>=<SID>escaped(line("'<"),line("'>"))<CR><Home>
nmap <buffer> ! .!
xmap <buffer> ! .!

function s:escaped(line1, line2) abort
  return range(a:line1, min([line('$'), a:line2]))
        \->map({ _, n -> fnameescape(s:line_get_relpath(n)) })
        \->join(' ')
endfunction

function s:line_get_relpath(n) abort
  let entry = s:oil.get_entry_on_line(0, a:n)
  if entry is v:null
    throw 'got nil oil entry on line ' .. a:n
  endif
  let fullpath = s:cwd .. entry.name
  let relpath = fnamemodify(fullpath, ':.')
  return fullpath is# relpath ? fullpath : '.' .. s:slash() .. relpath
endfunction

function s:slash() abort
  return !exists('+shellslash') || &shellslash ? '/' : '\'
endfunction
