" adapted from $VIMRUNTIME/ftplugin/help.vim

" collect \title and \(sub)*section invocations for a document outline.
" limitations:
" * does not check if within a comment/verbatim block/code block
" * cannot handle multiple headers on the same line: "\section{1} \section{2}"
" * assumes every invocation always uses braces so "\section a" won't work
" (should work in 99% cases still)

let s:document_title = 0

" lnum, col - position to start search for '{'
" (don't stand on '{')
" lines will be joined by space and trimmed
function s:get_braced_text(buf, lnum, col) abort
  let view = winsaveview()
  defer winrestview(view)
  call cursor(a:lnum, a:col)
  " lnum and col of the first character AFTER '{'
  " if '{' is the last character of a line, col will be col('$') + 1
  let [brace_start_lnum, brace_start_col] = searchpos('\v\{\zs(.|\n)')
  if brace_start_lnum == 0
        \ || getline('.')->matchstr('\%' .. brace_start_col .. 'c.') == '}'
    return ''
  endif
  " lnum and col of '}'
  let [brace_end_lnum, brace_end_col] = searchpos('}')
  let lines = getregion([a:buf, brace_start_lnum, brace_start_col, 0],
        \ [a:buf, brace_end_lnum, brace_end_col, 0],
        \ {'exclusive': 1})
  return lines
        \->map({ _, ln -> trim(ln) })
        \->filter( { _, ln -> !empty(ln) } )
        \->join()
endfunction

function s:mkentry(bufnr, lnum) abort
  let lnum = a:lnum
  let bufnr = a:bufnr
  let line = getline(lnum)
  let title_start = line->match('\v\\<title>')
  if title_start != -1
    let title = s:get_braced_text(bufnr, lnum, title_start + 1) " 1-based col
    let s:document_title = title
    return {'bufnr': bufnr, 'lnum': lnum, 'col': title_start + 1,
          \ 'text': toupper(title)}
  endif
  " matches \section* as well
  let section_start = line->match('\v\\<%(sub)*section>')
  if section_start != -1
    let subs = line->matchlist('\v\\<(%(sub)*)section>')[1]->len() / 3
    " one indent level for every 'sub'
    let indent = repeat('  ', subs)
    let section = indent .. s:get_braced_text(bufnr, lnum, section_start + 1)
    return {'bufnr': bufnr, 'lnum': lnum, 'col': section_start + 1,
          \ 'text': section}
  endif
endfunction

function s:show_toc() abort
  let bufnr = bufnr('%')
  let bufname = bufname(bufnr)
  let loclistwin = getloclist(0, {'winid': 1}).winid
  if loclistwin && loclistwin->getwinvar('qf_toc') ==# bufname
    lopen
    return
  endif
  let toc = range(1, line('$'))
        \->map({ _, lnum -> s:mkentry(bufnr, lnum) })
        \->filter({ _, e -> e isnot 0 })
  call setloclist(0, toc)
  let title = 'LaTeX TOC' ..
        \ (s:document_title is 0 ? '' : ' - "' .. s:document_title .. '"')
  call setloclist(0, [], 'a', {'title': title})
  lopen
  let w:qf_toc = bufname
endfunction

nnoremap <buffer> <silent> gO <CMD>call <SID>show_toc()<CR>
