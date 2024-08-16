" adapted from $VIMRUNTIME/ftplugin/help.vim

" collect \title and \(sub)*section invocations for a document outline.
" limitations:
" * does not check if within a comment/verbatim block/code block
" * cannot handle multiple headers on the same line: "\section{1} \section{2}"
" (should work in 99% cases still)

let s:document_title = 0

" lnum, col - position to start search for '{'
" (don't stand on '{')
function s:get_braced_text(lnum, col) abort
  let view = winsaveview()
  call cursor(a:lnum, a:col)
  let [brace_start_lnum, brace_start_col] = searchpos('{')
  if brace_start_lnum == 0
    return ''
  endif
  let [brace_end_lnum, brace_end_col] = searchpairpos('{', '', '}', 'n')
  call winrestview(view)
  let text = nvim_buf_get_text(0, brace_start_lnum - 1, brace_start_col,
        \ brace_end_lnum - 1, brace_end_col - 1, {})
  return join(text)
endfunction

function s:make_entry(lnum) abort
  let lnum = a:lnum
  let bufnr = bufnr('%')
  let line = getline(lnum)
  let title_start = line->match('\v\\<title>')
  if title_start != -1
    let title = s:get_braced_text(lnum, title_start + 1) " 1-based col
    let s:document_title = title
    return {'bufnr': bufnr, 'lnum': lnum, 'col': title_start + 1,
          \ 'text': toupper(title)}
  endif
  " matches \section* as well
  let section_start = line->match('\v\\<%(sub)*section>')
  if section_start != -1
    let subs = line->matchlist('\v\\<(%(sub)*)section>')[1]->len() / 3
    " every 'sub' turns into 2 spaces
    let indent = repeat('  ', subs)
    let section = indent .. s:get_braced_text(lnum, section_start + 1)
    return {'bufnr': bufnr, 'lnum': lnum, 'col': section_start + 1,
          \ 'text': section}
  endif
endfunction

function s:show_toc() abort
  let bufname = bufname('%')
  let info = getloclist(0, {'winid': 1})
  if !empty(info) && getwinvar(info.winid, 'qf_toc') ==# bufname
    lopen
    return
  endif
  let toc = range(1, line('$'))
        \->map({ _, lnum -> s:make_entry(lnum) })
        \->filter({ _, e -> e isnot 0 })
  call setloclist(0, toc)
  let title = 'LaTeX TOC' ..
        \ (s:document_title is 0 ? '' : ' - "' .. s:document_title .. '"')
  call setloclist(0, [], 'a', {'title': title})
  lopen
  let w:qf_toc = bufname
endfunction

nnoremap <buffer> <silent> gO <CMD>call <SID>show_toc()<CR>
