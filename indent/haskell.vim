if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

setlocal indentexpr=s:get_indent()
setlocal indentkeys=!^F,o,O,=where,=deriving,==,0=in,0=class,0=instance,0=data,0=type,0=else,0<bar>,0},0],0(,0),0#,0,0==

let s:save_cpo = &cpo
set cpo&vim

" the main function.
" if the line is empty, choose the farthest indent;
" else if the line has been indented in a suitable position already, keep it;
" else if the line is between two indents, push to the farther one;
" else if the line is indented outside the farthest possible indent, push it
" back
function! s:get_indent() abort
  let possible_indents = s:get_indents()->sort()
  if empty(possible_indents)
    return -1
  endif
  let line = getline(v:lnum)
  let indent = indent(v:lnum)
  if line =~# '\s*'
    return possible_indents[-1]
  endif
  for pindent in possible_indents
    if pindent >= indent
      return pindent
    endif
  endfor
  return possible_indents[-1]
endfunction

" get all possible levels of indentation
" example: given last line "case3 x = case x of Just n  -> case n of 1 -> 1",
" it is possible to start next line at line start, 10th or 31st position
" hence [0, 10, 31] is returned
function! s:get_indents() abort
  return []
endfunction

" get all matches for a pattern in the string
function! s:matches(str, pat) abort
  let count = 1
  let ret = []
  while 1
    let res = match(a:str, a:pat, 0, count)
    if res == -1
      return ret
    endif
    let count += 1
    call add(ret, res)
  endwhile
endfunction

" prevnonblank while also skipping macros
function! s:prevnonblank(lnum) abort
  let i = a:lnum
  while i > 0
    let i = prevnonblank(i)
    if getline(i) !~# '\v^\s*#\s*\w+'
      return i
    endif
    let i -= 1
  endwhile
  return 0
endfunction

function! s:indent_comment() abort
  if getline(s:prevnonblank(v:lnum - 1)) =~# '\v\{-#\s*UNPACK\s*#-}' && getline(v:lnum) =~# '\v^\s*\{-#\s*UNPACK\s*#-}'
    return match(getline(s:prevnonblank(v:lnum - 1)), '\v\{-#\s*UNPACK\s*#-}')
  elseif getline(v:lnum) =~# '\v^\s*\{-#\s*<RULES>\s*%(--.*)?$'
    let name = matchstr(getline(v:lnum + 1), '\v^\s*"\zs\k+\ze%(/\k+)*"')
    if name !=# ''
      let i = v:lnum - 1
      while i
        if getline(i) =~# '\v^\s*%(where\s+)?<' . name . '>.*\='
          return match(getline(i), '\v^\s*%(<where>)?\s*\zs')
        endif
        let i -= 1
      endwhile
    endif
  endif
  if getline(v:lnum) =~# '\v^\s*\{-#\s*<%(INLINE|RULES)>'
    return -1
  elseif getline(v:lnum) =~# '\v^\s*%(\{- \||\{-#.*#-}\s*%(--.*)?$|-- -{10,})'
    return 0
  endif
  if getline(v:lnum) =~# '^\s*[-{]-'
    let i = v:lnum
    if getline(i) =~# '^\s*--'
      while i <= line('$') && (getline(i) =~# '^\s*--' || getline(i) ==# '')
        let i += 1
      endwhile
      if getline(i) =~# '\v^\s*<%(class|instance|data)>|::.*%(-\>|-- *\^)'
        return match(getline(i), '^\s*\zs\S')
      endif
    endif
    let i = s:prevnonblank(v:lnum - 1)
    let previndent = 0
    while i > 0
      let line = getline(i)
      let indent = indent(i)
      if line =~# '^\s*[-{]-'
        return indent
      elseif line =~# '\v^\s*<%(class|instance)>|^\s*<where>\s*%(--.*)?$' && line !~# '\v,\s*%(--.*)?$'
        return indent + &shiftwidth
      elseif line =~# '\v\s*\(\s*%(--.*)?$'
        return previndent ? previndent : indent + &shiftwidth
      elseif line =~# '^\S' && line !~# '^\s*#'
        return 0
      endif
      let previndent = indent
      let i -= 1
    endwhile
  endif
  let listpattern = '\v^\s*%(\* \@|[a-z]\)\s+|\>\s+)'
  if getline(v:lnum) =~# listpattern
    if getline(s:prevnonblank(v:lnum - 1)) =~# listpattern
      return indent(s:prevnonblank(v:lnum - 1))
    else
      if getline(v:lnum) =~# '\v^\s*[a-z]\)\s+'
        let i = s:prevnonblank(v:lnum - 1)
        let indent = indent(i)
        while 0 < i && indent(i) == indent
          let i -= 1
        endwhile
        if 0 < i && getline(i) =~# '\v^\s*[a-z]\)\s+'
          return indent(i)
        endif
      endif
      return indent(s:prevnonblank(v:lnum - 1)) + &shiftwidth
    endif
  endif
  if getline(v:lnum - 1) =~# '\v^\s*[a-z]\)\s+'
    return match(getline(v:lnum - 1), '\v^\s*[a-z]\)\s+\zs')
  endif
  if getline(v:lnum) !~# '\v^\s*%(--.*)?$' && getline(s:prevnonblank(v:lnum - 1)) =~# listpattern
    return indent(s:prevnonblank(v:lnum - 1)) - &shiftwidth
  endif
  if getline(v:lnum) =~# '^\s*[-{]-'
    return 0
  endif
  let line = getline(s:prevnonblank(v:lnum - 1))
  if line =~# '\v^\s*\{-#\s*%(\s+\w+,?)+'
    if line =~# '\v,\s*%(--.*)?$'
      return match(line, '\v\zs<\w+,')
    else
      return match(line, '\v\w+\s+\zs<\w+') - &shiftwidth
    endif
  endif
  let i = s:prevnonblank(v:lnum - 1)
  if i < v:lnum - 1
    let indent = indent(i)
    while 0 < i && indent(i) == indent
      let i -= 1
    endwhile
    if 0 < i && getline(i) =~# '\v^\s*[a-z]\)\s+'
      return indent(i) - &shiftwidth
    endif
  endif
  if getline(v:lnum) =~# '\v^\s*%(#?-}|#$)'
    let i = v:lnum - 1
    while 0 < i
      if getline(i) =~# '{-'
        return match(getline(i), '{-')
      endif
      let i -= 1
    endwhile
  endif
  return indent(s:prevnonblank(v:lnum - 1))
endfunction

function! s:in_comment() abort
  if getline(v:lnum) =~# '^\s*--'
    return 1
  endif
  let start = searchpos('\v%(--.*)@<!\{-', 'bcnW')
  let pos = getpos('.')
  let end = searchpos('-}', 'bcnW')
  return start != [0, 0] && (start[0] < pos[1] || start[0] == pos[1] && start[1] <= pos[2])
        \ && (end == [0, 0] || end[0] < start[0] || end[0] == start[0] && end[1] < start[1])
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
