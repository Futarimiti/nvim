let &tabline = $'%!{expand('<SID>')}tabline()'

function s:tabline() abort
  let tablist = gettabinfo()
        \->map({ _, tab -> s:tabsegment(tab.tabnr) })
        \->join()
  let lhs = tablist
  let rhs = [s:showcmd(), s:macro()]
        \->filter({ _, s -> !empty(s) })
        \->join()
  return $'{lhs}%={rhs}'
endfunction

function s:showcmd() abort
  return &showcmdloc is 'tabline' ? '%S' : ''
endfunction

function s:macro() abort
  let name = reg_recording()
  if empty(name)
    return ''
  else
    let hl = 'Normal'
    return $'%#{hl}#[@{name}]'
  endif
endfunction

function s:obsession() abort
  packadd vim-obsession
  let status = ObsessionStatus()
  let hl = status is '[$]' ? 'ObsessionActive' : 'ObsessionInactive'
  return $'%#{hl}#{status}%#TabLine#'
endfunction

function s:tabsegment(tabnr) abort
  let hl = a:tabnr is tabpagenr() ? 'TabLineSel' : 'TabLine'
  let name = s:tabname(a:tabnr)->statusline#escape()
  return $'%#{hl}#%{a:tabnr}T {name}%#TabLine#'
endfunction

" work out the display name of a tabpage
function s:tabname(tabnr) abort
  let buf = tabpagebuflist(a:tabnr)[tabpagewinnr(a:tabnr) - 1]
  let buftype = buf->getbufvar('&buftype')

  if empty(buftype)
    let bname = buf->bufname()
    if empty(bname)
      return '[No Name]'
    endif
    let relname = bname->fnamemodify(':t')
    return relname .. (isdirectory(bname) ? '/' : '')
  elseif buftype is 'terminal'
    let chan = buf->getbufvar('&channel')
    call assert_notequal(chan, 0)
    let cmd = nvim_get_chan_info(chan)->get('argv', [])
    let cmdname = cmd->get(-1, '')->split()->get(0, '')
    let tail = cmdname->fnamemodify(':t')
    return empty(cmdname) ? '[terminal]' : $'!{tail}'
  elseif buftype is 'help'
    let bname = buf->bufname()
    let tail = bname->fnamemodify(':t')
    return $':h {tail}'
  elseif buftype is 'acwrite' && buf->getbufvar('&filetype') is 'oil'
    " oil:///home/...
    " ^^^^^^
    return buf->bufname()[6:]->simplify()->fnamemodify(':.') ?? '.'
  else
    return $'[{buftype}]'
  endif
endfunction

augroup RedrawTabline
  autocmd!
  autocmd RecordingEnter,RecordingLeave * redrawtabline
augroup END

