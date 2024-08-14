" adapted from https://github.com/itchyny/vim-haskell-indent

if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

setlocal indentexpr=s:get_indent()
setlocal indentkeys=!^F,o,O,=where,=deriving,==,0=in,0=class,0=instance,0=data,0=type,0=newtype,0=else,0<bar>,0},0],0(,0),0#,0,0==,0==>,0=->

let s:save_cpo = &cpo
set cpo&vim

" the main function.
" if the line is empty, choose the farthest indent;
" else if the line has been indented in a suitable position already, keep it;
" else if the line is between two indents, push to the farther one;
" else if the line is indented outside the farthest possible indent, push back
function! s:get_indent() abort
  let raw_indents = s:get_indents()
  if type(raw_indents) == v:t_number
    return raw_indents
  endif
  assert_equal(v:t_list, type(raw_indents))
  let possible_indents = sort(raw_indents)->uniq()
  if empty(possible_indents)
    return -1
  endif
  let line = getline(v:lnum)
  let indent = indent(v:lnum)
  if line =~# '^\s*$'
    return possible_indents[-1]
  endif
  for pindent in possible_indents
    if pindent >= indent
      return pindent
    endif
  endfor
  return possible_indents[-1]
endfunction

" get all possible levels of indentation, return a list or a single number.
" example: given last line "case3 x = case x of Just n  -> case n of 1 -> 1",
" it is possible to start next line at line start, 10th or 31st position
" hence [0, 10, 31] is returned
function! s:get_indents() abort
  let line = getline(v:lnum)

  " comment
  if s:in_comment()
    let i = s:indent_comment()
    if i >= 0
      return i
    endif
  endif

  " quasiquotes
  if s:in_quasiquotes() && line !~ '\v^\s*\[.{-}\|'
    return -1
  endif

  " #if, #else, #endif, #include
  if line =~# '\v^\s*%(#$|#\s*\w+)'
    return 0
  endif

  " where
  if line =~# '\v<where>'
    let i = s:indent_where()
    if i >= 0
      return i
    endif
  endif

  " class, instance
  if line =~# '\v^\s*<%(class|instance)>'
    return 0
  endif

  " else
  if line =~# '\v^\s*<else>'
    return s:indent_else()
  endif

  " |
  if line =~# '\v^\s*\||\|\s*%(--.*)?$'
    return s:indent_bar()
  endif

  " in
  if line =~# '\v^\s*<in>'
    return s:indent('\v^\s*<in>', '\v^.*<let>\s*\zs', 0, -1)
  endif

  " }, ], )
  if line =~# '\v^\s*[})\]]'
    return s:indent_parenthesis()
  endif

  " first non-blank line
  if s:prevnonblank(v:lnum - 1) == 0
    return 0
  endif

  let nonblankline = getline(s:prevnonblank(v:lnum - 1))

  " deriving
  if line =~# '\v^\s*<deriving>'
    if line =~# '\v^\s*}\s*deriving>'
      return s:indent_parenthesis()
    endif
    if nonblankline =~# '\v^\s*\|'
      return match(nonblankline, '\v^\s*\zs\|')
    endif
    return s:indent('\v<deriving>', '\v^.*\zs<(data|newtype)>.*\=', &shiftwidth)
  endif

  " ^=>, ^->
  if line =~# '\v^\s*\=\>' || line =~# '\v^\s*-\>'
    for pat in ['::', '=>', '->']
      let i = match(nonblankline, pat)
      if i != -1
        return i
      endif
    endfor
    throw '=>/-> not immediately following ::/->/=>'
  endif

  " =
  if line =~# '\v^\s*\='
    return match(nonblankline, '\v^\s*%(<where>|<let>)?\s*\zs') + &shiftwidth
  endif

  " data, type
  if line =~# '\v^\s*<%(data|type|newtype)>' && nonblankline !~# '\v<%(class|instance)>.*<where>'
    return 0
  endif

  let noparen = '[^()[\]{}]'
  let noparen = '%(' . noparen . '+|\(' . noparen . '*\)|\['  . noparen . '*\])'
  let noparen = '%(' . noparen . '+|\(' . noparen . '*\)|\['  . noparen . '*\])*'

  if line =~# '\v^\s*,' . noparen . '\s*%(--.*)?$' && nonblankline =~# '\v^\s*,'
    return match(nonblankline, '^\s*\zs,')
  endif

  let prevline = getline(v:lnum - 1)

  " after a multilined type signature
  if nonblankline =~# '\v^\s+[-=]\>'
    " search upwards until found ::
    let decl = -1
    let curr = v:lnum
    while curr > 0
      if getline(curr) =~# '::'
        let decl = curr
        break
      endif
      let curr -= 1
    endwhile
    if decl != -1
      return indent(decl)
    endif
  endif

  " inside #if, #else, #endif, #include
  if nonblankline =~# '^\s*#'
    return 0
  endif

  " after a line comment
  if nonblankline =~# '^\s*--'
    return match(nonblankline, '^\s*\zs--')
  endif

  if nonblankline =~# '\v^\s*}?' . noparen . '[([{]' . noparen . '[-+/*\$&<>,]?\s*%(--.*)?$'
    if nonblankline =~# '\v[([{]\s*%(--.*)?$'
      return match(nonblankline, '\v^\s*%(<where>|.*<let>)?\s*\zs') + &shiftwidth
    elseif nonblankline =~# '\v[-+/*\$&<>,]\s*%(--.*)?$'
      return match(nonblankline, '\v^\s*}?' . noparen . '%(\[.*\|\s*\zs|[([{]\s*\zs)')
    elseif nonblankline =~# '\v^[^[\]]*\[([^[\]]*|\[[^[\]]*\])*\|%([^[\]]*|\[[^[\]]*\])*\s*%(--.*)?$'
      return match(nonblankline, '\v^[^[\]]*\[%([^[\]]*|\[[^[\]]*\])*\zs\|')
    else
      return match(nonblankline, '\v^\s*}?' . noparen . '\zs[([{]')
    endif
  endif

  " (
  if line =~# '\v^\s*\('
    let lnum = s:prevnonblank(v:lnum - 1)
    if lnum == 0
      return -1
    elseif nonblankline =~# '\v^\s*%(<where>|.*<let>).*%([-+/*\$&<>=,]+|`\k+`)\s*%(--.*)?$'
      return match(nonblankline, '\v^\s*<%(where|let)>\s*\zs') + &shiftwidth
    elseif nonblankline =~# '\v^\s*<%(where|let)>'
      return match(nonblankline, '\v^\s*%(<where>|<let>)?\s*\zs')
    elseif nonblankline =~# '\v^\s*<import>'
      return indent(lnum) + &shiftwidth
    endif
  endif

  if nonblankline =~# '\v^\s*<infix[rl]?>'
    return match(nonblankline, '\S')
  endif

  if nonblankline =~# '\v^\s*<instance>.*\=\>\s*%(--.*)?$'
    return match(nonblankline, '\v^\s*\zs<instance>') + &shiftwidth
  endif

  if nonblankline =~# '\v<do>\s*%(--.*)?$'
    return match(nonblankline, '\v^\s*%(<where>|.*<let>)?\s*\zs') + &shiftwidth
  endif

  " $ for TemplateHaskell just in case
  if nonblankline =~# '\v<do>\s*[[:alnum:](_\-\"\''\[\$]'
    return match(nonblankline, '\v<do>\s*\zs\S')
  endif

  if nonblankline =~# '\v<deriving>'
    return s:indent('', '\v^\s*\zs<(data|newtype)>', 0)
  endif

  if prevline =~# '\v<if>' && prevline !~# '\v^\s*#'
    if prevline !~# '\v<then>'
      return match(prevline, '\v.*<if>\s*\zs')
    elseif prevline !~# '\v<else>'
      return match(prevline, '\v.*\zs<then>')
    endif
  endif

  " case (v :: Void) of {}
  " case x of { 1 -> 1; _ -> 0 }
  if prevline =~# '\v<case>.*<of>\s*\{[^\}]*\}' && prevline !~# '^\s*#'
    return -1
  endif

  " case x of ...
  if prevline =~# '\v<case>.*<of>\s*[[:alnum:](\-\"''\[]' && prevline !~# '^\s*#'
    return s:matches(prevline, '\v<case>.{-}<of>\s*\zs\S')->add(0)
  endif

  " case x of
  " empty case allowed
  if prevline =~# '\v<case>.*<of>\s*%(--.*)?$' && prevline !~# '^\s*#'
    return match(prevline, '\v.*\zs<case>\s*') + &shiftwidth
  endif

  if prevline =~# '\v\\\s*(<case>|<cases>)\s*%(--.*)?$'
    return match(prevline, '\v^\s*%(<where>|.*<let>)?\s*\zs') + &shiftwidth
  endif

  " use of _ in \case means this is last case
  if prevline =~# '\v\\\s*<case>\s*[[:alnum:](\-\"''\[]'
    return match(prevline, '\v\\\s*<case>\s*\zs\S')
  endif

  " use of _ in \cases does not necessarily mean this is last case
  if prevline =~# '\v\\\s*<cases>\s*[[:alnum:](_\-\"''\[]'
    return match(prevline, '\v\\\s*<case>\s*\zs\S')
  endif

  if nonblankline =~# '\v^.*[^|]\|[^|].*\='
    return s:after_guard()
  endif

  " multiway-if entry
  if nonblankline =~# '\v\|.*-\>.*' && prevline =~# '\v^\s*%(--.*)?$'
    return [0, match(prevline, '\v^\s*%(where\s+)?\zs')]
  endif

  " case entry
  " listing all possible indents, may be costy
  if nonblankline =~# '\v-\>' && prevline =~# '\v^\s*%(--.*)?$'
        \ || nonblankline =~# '\v^\s*_\s*-\>'
    let i = s:prevnonblank(v:lnum - 1)
    let ret = [0]
    while i
      let prevline = getline(i)
      if prevline =~# '\v\\\s*(<case>|<cases>)'
        " \case \cases
        let ret += s:matches(prevline, '\v\\\s*<case>\s*\zs')
      endif
      
      if prevline =~# '\v<case>.{-}<of>\s*%(--.*)?$'
        " case x of$
        let ret += s:matches(prevline,'\v<case>\s*\zs.{-}<of>\s*%(--.*)?$')
      endif

      if prevline =~# '\v<case>.{-}<of>\s*[[:alnum:](\-\"''\[]'
        " case x of ...
        let ret += s:matches(prevline, '\v<case>.{-}<of>\s*\zs\S')
      endif
      let i -= 1
    endwhile
    return ret
  endif

  " TODO: relax this - see FIX marks in Test.hs
  if nonblankline =~# '\v[)}\]]\s*%(--.*)?$'
    return -1
    " return s:unindent_after_parenthesis(s:prevnonblank(v:lnum - 1), match(nonblankline, '\v[)}\]]\s*%(--.*)?$'))
  endif

  if nonblankline =~# '\v^\s*\|\s*.*\<-\s*.*,\s*%(--.*)?$'
    return match(nonblankline, '\v^\s*\|\s*\zs.*\<-\s*.*,\s*%(--.*)?$')
  endif

  if nonblankline =~# '\v([-+/*\$&<>=,]+|`\k+`)\s*%(--.*)?$'
    if nonblankline =~# '\v^\s*<let>.*,\s*%(--.*)?$'
      return match(nonblankline, '\S')
    else
      return match(nonblankline, '\v^\s*%(<where>|.*<let>)?\s*\zs') +
            \ (nonblankline =~# '\v%(<where>|<let>)|^\s*\k+\s*'. noparen .'\=.*%([-+/*\$&<>]|`\k+`)\s*%(--.*)?$|%(\=|-\>)\s*%(--.*)?$' ? &shiftwidth : 0)
    endif
  endif

  if nonblankline =~# '\v<where>'
    return s:after_where()
  endif

  if nonblankline =~# '\v<module>' && nonblankline !~# '\v,\s*%(--.*)?$' && indent(s:prevnonblank(v:lnum - 1)) < &shiftwidth
    return &shiftwidth
  endif

  if nonblankline =~# '\v<else>'
    let i = s:prevnonblank(v:lnum - 1)
    while i
      let prevline = getline(i)
      if prevline =~# '\v<if>'
        if prevline =~# '\v^\s*_\s*-\>'
          " ?
          let nonblankline = prevline
          break
        endif
        return match(prevline, '\v^\s*\zs')
      endif
      let i -= 1
    endwhile
  endif

  if nonblankline =~# '\v^\s*<(data|newtype)>.*\='
    if nonblankline =~# '\v\{-#\s*UNPACK\s*#-}' && getline(v:lnum) =~# '\v^\s*\{-#\s*UNPACK\s*#-}'
      return match(nonblankline, '\v\{-#\s*UNPACK\s*#-}')
    endif
    return match(nonblankline, '\v^\s*\zs<(data|newtype)>') + &shiftwidth
  endif

  if nonblankline =~# '\v<let>\s+.*\=' && nonblankline !~# '\v<let>\s+.*\=.*<in>'
    return s:indent('', getline(v:lnum) =~# '\v^\s*%(<in>|\S+\s*\=)' ? '\v<let>\s+\zs\S' : '\v<let>', 0)
  endif

  " in
  if nonblankline =~# '\v^\s*<in>'
    return s:indent('', '\v^\s*\zs.*<let>', 0, -1)
  endif

  " after signature
  if nonblankline =~# '::'
    return s:indent('', nonblankline =~# '\v,\s*%(--.*)?$' ? '\S' : '\v\{\s*\<\w+\s*::', 0, match(nonblankline, '\S'))
  endif

  if s:prevnonblank(v:lnum - 1) < v:lnum - 2 && prevline !~# '^\s*#'
    return 0
  elseif s:prevnonblank(v:lnum - 1) < v:lnum - 1 && prevline !~# '^\s*#'
    let i = s:prevnonblank(v:lnum - 1)
    let where_clause = 0
    let found_where = v:false
    let indent = indent(s:prevnonblank(v:lnum - 1))
    while i
      let prevline = getline(i)
      if substitute(prevline, '--.*', '', 'g') =~# '\v<where>'
        let found_where = v:true
        if indent(i) <= indent
          let where_clause += 1
          if where_clause == v:lnum - s:prevnonblank(v:lnum - 1)
            return [0, match(prevline, '\v^.*<where>\s*\zs')]
          endif
        endif
      endif
      if 0 <= indent(i)
            \ && indent(i) < indent
            \ && prevline !~# '\v<where>|^\s*\||^$'
        return [0, prevline =~# '\v^\s*[([{]' ? indent : indent(i)]
      endif
      if prevline =~# '\v^\s*<%(class|instance)>' && found_where
        " empty class/instance is allowed
        return [0, match(prevline, '\v^\s*<%(class|instance)>') + &shiftwidth]
      elseif prevline =~# '^\S'
        return 0
      endif
      let i -= 1
    endwhile
    return 0
  endif

  " not sure what this do TODO
  let prevnonblank = s:prevnonblank(v:lnum - 1)
  let prevprevnonblank = s:prevnonblank(prevnonblank - 1)
  if indent(prevprevnonblank) < indent(prevnonblank)
        \ && nonblankline =~# '\v^\s*[-+/*$&<>=]'
        \ || getline(prevprevnonblank) =~# '\v\=\s*%(--.*)?$'
    return [0, indent(prevnonblank), indent(prevprevnonblank)]
  endif

  " last resort
  return indent(prevnonblank)
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

" a general indent function by searching the pattern upward
function! s:indent(linepattern, pattern, diff, ...) abort
  let i = s:prevnonblank(v:lnum - 1)
  if i < v:lnum - 1
    return 0
  endif
  if getline(v:lnum) =~# a:linepattern
    while i > 0
      let line = getline(i)
      if line =~# a:pattern
        return match(line, a:pattern) + a:diff
      elseif line =~# '^\S'
        return a:0 ? a:1 : -1
      endif
      let i -= 1
    endwhile
  endif
  return -1
endfunction

" the cursor is in comment
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

" [| the cursor is in template haskell quasiquotes |]
" [quasiquoter| █ |] => 1
" [quasiquoter█| |] => 0
function! s:in_quasiquotes() abort
  " if getline(v:lnum) =~# '\v^\s*[.{-}\|'
  "   return 1
  " endif
  let start = searchpos('\v[.{-}\|', 'bcnW')
  if start == [0, 0]
    return 0
  endif
  let end = searchpos('|]', 'bcnW')
  if end == [0, 0]
    return 1
  elseif start[0] > end[0]
    return 1
  elseif start[0] == end[0] && end[1] < start[1]
    return 1
  else
    return 0
  endif
endfunction

" comment
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
      if getline(i) =~# '\v^\s*<%(class|instance|data|newtype)>|::.*%(-\>|-- *\^)'
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

" else
function! s:indent_else() abort
  let i = s:prevnonblank(v:lnum - 1)
  while i > 0
    let line = getline(i)
    if line =~# '\v<then>'
      return match(line, '\v<then>')
    endif
    let i = s:prevnonblank(i - 1)
  endwhile
endfunction

" |
function! s:indent_bar() abort
  if getline(v:lnum) !~# '^\s*|'
    return -1
  endif
  let i = s:prevnonblank(v:lnum - 1)
  let indent = indent(i)
  while i > 0
    let line = getline(i)
    if line =~# '\v^[^[\]]*%([^[\]]*|\[[^[\]]*\])*\[%([^[\]]*|\[[^[\]]*\])*%(--.*)?$'
      return match(line, '\v^[^[\]]*%([^[\]]*|\[[^[\]]*\])*\zs\[([^[\]]*|\[[^[\]]*\])*%(--.*)?$') + &shiftwidth
    elseif line =~# '\v<if>\s*\|'
      " multiway if
      return match(line, '\v<if>\s*\zs\|')
    elseif line =~# '\v\\\s*<case>.*\|'
      " \case
      return match(line, '\v\\\s*<case>.*\zs\|')
    elseif line =~# '\v\\\s*<cases>.*\|'
      " \cases
      return match(line, '\v\\\s*<cases>.*\zs\|')
    elseif line =~# '\v^\s*%(<where>)?.*[^|]\|[^|].*\='
      return match(line, '\v^\s*%(<where>)?.*[^|]\zs\|[^|].*\=')
    elseif line =~# '\v<(data|newtype)>.*\='
      return match(line, '\v^.*<(data|newtype)>.*\zs\=')
    elseif line =~# '\v^\s*<where>\s*%(--.*)?$' && indent(i) < indent
          \ || line =~# '^\S'
      return indent + &shiftwidth
    elseif line =~# '\v^\s*<where>\s+\S'
      return match(line, '\v^\s*<where>\s+\zs\S') + &shiftwidth
    elseif line =~# '\v[^|]\|[^|].*-\>'
      return match(line, '\v[^|]\zs\|[^|].*-\>')
    elseif line =~# '^\s*='
      return match(line, '^\s*\zs=')
    endif
    let indent = indent(i)
    let i = s:prevnonblank(i - 1)
  endwhile
endfunction

" guard
" FIX - see fix mark
function! s:after_guard() abort
  let nonblankline = getline(s:prevnonblank(v:lnum - 1))
  let line = getline(v:lnum - 1)
  if line =~# '\v^\s*%(--.*)?$'
    if s:prevnonblank(v:lnum - 1) < v:lnum - 2
      return 0
    endif
    let i = v:lnum - 1
    let where_clause = 0
    while i
      let line = getline(i)
      if line =~# '^\S'
        return 0
      endif
      if where_clause && line !~# '\v^\s*%(--.*)?$' && line !~# '\v^\s*\|[^|]'
        return match(line, '\v^\s*%(<where>)?\s*\zs')
      endif
      if line =~# '\v<where>'
        let where_clause = 1
      endif
      let i -= 1
    endwhile
  endif
  if nonblankline =~# '\v[^|]\|\s*%(otherwise|True|0\s*\<\s*1|1\s*\>\s*0)' || getline(v:lnum) =~# '^\s*\S'
    let i = s:prevnonblank(v:lnum - 1)
    while i
      let line = getline(i)
      if line !~# '\v^\s*%(--.*)?$' && line !~# '^\s*|'
        return match(line, '\v^\s*%(<where>)?\s*\zs')
      endif
      let i -= 1
    endwhile
  elseif nonblankline =~# '\v^\s*<(data|newtype)>.*\='
    return match(line, '\v^.*<(data|newtype)>.*\zs\=')
  else
    return match(line, '\v^.*[^|]\zs\|[^|].*\=')
  endif
endfunction

" }, ], )
" TODO: use searchpairpos() instead of trickries
function! s:indent_parenthesis() abort
  let view = winsaveview()
  execute 'normal! ' v:lnum . 'gg^'
  let end = getpos('.')
  normal! %
  let begin = getpos('.')
  call setpos('.', end)
  call winrestview(view)
  if begin[1] == end[1]
    return -1
  endif
  if indent(end[1] - 1) + 1 < begin[2]
    return match(getline(begin[1]), '\v^\s*%(<where>|.*<let>)?\s*\zs')
  endif
  if getline(end[1]) =~# '^\s*}' && getline(begin[1]) =~# '\v^\s+\w+\s*\{'
    return match(getline(begin[1]), '\v^\s+\zs')
  endif
  return begin[2] - 1
endfunction

" unindent after closed parenthesis
function! s:unindent_after_parenthesis(line, column) abort
  let i = s:prevnonblank(v:lnum - 1)
  if i < v:lnum - 2
    return 0
  endif
  let pos = getpos('.')
  let view = winsaveview()
  execute 'normal! ' a:line . 'gg' . (a:column + 1)  . '|'
  normal! %
  let begin = getpos('.')
  call setpos('.', pos)
  call winrestview(view)
  if getline(begin[1]) =~# '\v<deriving>'
    let i = begin[1]
    while i
      let line = getline(i)
      if line =~# '\v<(data|newtype)>'
        return match(line, '\v<(data|newtype)>')
      elseif line =~# '^\S'
        return -1
      endif
      let i -= 1
    endwhile
  elseif getline(begin[1]) =~# '^\s*='
    return match(getline(s:prevnonblank(begin[1] - 1)), '\v^\s*%(<where>|<let>)?\s*\zs')
  elseif getline(s:prevnonblank(begin[1] - 1)) =~# '\v\=\s*%(--.*)?$'
    return match(getline(s:prevnonblank(begin[1] - 1)), '\v^\s*%(<where>|<let>)?\s*\zs')
  elseif getline(s:prevnonblank(begin[1] - 1)) =~# '\v<import>'
    return 0
  endif
  return [0, match(getline(begin[1]), '\v^\s*%(<where>)?\s*\zs')]
endfunction

" where
function! s:indent_where() abort
  if getline(v:lnum) =~# '\v^\s*<where>'
    let i = s:prevnonblank(v:lnum - 1)
    while i > 0
      let line = getline(i)
      if line =~# '\v^\s*%(<where>)?\s*\zs\h.*\=|^\s*[^| ]'
        return match(line, '\v^\s*%(<where>)?\s*\zs\h.*\=|^\s*[^| ]') + &shiftwidth
      elseif line =~# '^\S'
        return -1
      endif
      let i -= 1
    endwhile
  elseif getline(v:lnum) =~# '\v^\s*\)\s*<where>'
    let pos = getpos('.')
    let view = winsaveview()
    execute 'normal! ' (match(getline(v:lnum), ')') + 1)  . '|%'
    let begin = getpos('.')
    call setpos('.', pos)
    call winrestview(view)
    if getline(begin[1]) =~# '\v<%(module|class|instance)>'
      return indent(begin[1]) + &shiftwidth
    elseif getline(s:prevnonblank(begin[1] - 1)) =~# '\v<%(module|class|instance)>'
      return indent(s:prevnonblank(begin[1] - 1)) + &shiftwidth
    elseif getline(begin[1]) =~# '\v^\s*\(%(--.*)?'
      return indent(begin[1])
    endif
  elseif getline(v:lnum) =~# '\v^\s*<%(module|class|instance)>'
    return 0
  elseif getline(v:lnum) =~# '\v<where>\s*%(--.*)?'
    let i = s:prevnonblank(v:lnum - 1)
    if i > 0
      let line = getline(i)
      if line =~# '\v^\s*<%(module|class|instance)>'
        return indent(i) + &shiftwidth
      endif
    endif
  endif
  return -1
endfunction

" where
function! s:after_where() abort
  let line = getline(s:prevnonblank(v:lnum - 1))
  if line =~# '\v^\s*\)\s*<where>\s*%(--.*)?$'
    let pos = getpos('.')
    let view = winsaveview()
    execute 'normal! ' s:prevnonblank(v:lnum - 1) . 'gg^%'
    let begin = getpos('.')
    call setpos('.', pos)
    call winrestview(view)
    let i = getline(begin[1]) =~# '^\s*(' ? s:prevnonblank(begin[1] - 1) : begin[1]
    if i > 0 && getline(i) =~# '\v<%(module|class|instance)>'
      return 0
    endif
  endif
  if line =~# '\v<where>\s*%(--.*)?$'
    let i = s:prevnonblank(v:lnum - 1)
    while i > 0
      let line = getline(i)
      if line =~# '\v^\s*<module>'
        return 0
      elseif line =~# '\v^\s*%(<class>|<instance>|<data>|<newtype>|<type> +<family>)'
        if line =~# '\v<where>\s*%(--.*)?$' && i != s:prevnonblank(v:lnum - 1)
          break
        endif
        return match(line, '\v%(<class>|<instance>|<data>|<newtype>|<type> +<family>)') + &shiftwidth
      elseif line =~# '\v^%(\S|\s*\k+\s*\=)' && line !~# '^--'
        return match(getline(s:prevnonblank(v:lnum - 1)), '\v<where>') + &shiftwidth
      endif
      let i -= 1
    endwhile
  endif
  if line =~# '\v^\s*<where>'
    if s:prevnonblank(v:lnum - 1) < v:lnum - 2
      return 0
    elseif s:prevnonblank(v:lnum - 1) < v:lnum - 1
      let i = s:prevnonblank(v:lnum - 1) - 1
      let indent = indent(s:prevnonblank(v:lnum - 1))
      while i
        let line = getline(i)
        if line =~# '^\S'
          return 0
        elseif indent(i) < indent
          return match(line, '\v^\s*%(<where>)?\s*\zs')
        endif
        let i -= 1
      endwhile
      return 0
    endif
    return match(line, '\v<where>\s*\zs')
  endif
  if getline(s:prevnonblank(v:lnum - 1)) =~# '\v^\s*<where>\s*%(--.*)?$'
    return indent(s:prevnonblank(v:lnum - 1)) + &shiftwidth
  endif
  return indent(s:prevnonblank(v:lnum - 1))
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
