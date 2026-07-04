set laststatus=3
set noruler
let &statusline = $'%!{expand('<SID>')}statusline()'

function s:statusline() abort
  let buf = bufnr()
  let lhs = s:lhs(buf)->join('')
  let rhs = s:rhs(buf)->join('')
  return $'{lhs}%={rhs}'
endfunction

function s:lhs(buf) abort
  let buftype = a:buf->getbufvar('&buftype')
  " %< marks start of truncating long text
  return buftype is 'terminal'
        \ ? ['!', '%<', s:cmdname(a:buf)->statusline#escape()]
        \ : ['%<', s:bufdisp(a:buf)->statusline#escape(), ' ', '%w', '%m']
endfunction

function s:rhs(buf) abort
  let fugitive = s:fugitive()
  let obsession = s:obsession()
  let watch = s:watch()
  return [watch, '%h', '%r', fugitive, obsession]
endfunction

""" components

" treat given buf as terminal buffer (panics if not), get name of cmd
function s:cmdname(buf) abort
  let chan = a:buf->getbufvar('&channel')
  call assert_notequal(chan, 0)
  let cmd = nvim_get_chan_info(chan)->get('argv', [])
  return cmd->get(-1, '')->fnamemodify(':.')
endfunction

" work out a proper display name for a buffer
function s:bufdisp(buf) abort
  let bufname = a:buf->bufname()
  if empty(bufname)
    return '[No Name]'
  endif
  let buftype = a:buf->getbufvar('&buftype')
  let filetype = a:buf->getbufvar('&filetype')
  " simplify bufname only if is a normal buffer
  if buftype is ''
    return bufname->simplify()->fnamemodify(':.')
  elseif buftype is 'help'
    " only leave the last component (xxx.txt) for help buffer
    return bufname->simplify()->fnamemodify(':t')
  elseif buftype is 'nowrite'
    if filetype is 'directory'
      return bufname[-1:] is '/' ? bufname : bufname .. '/'
    endif
  elseif buftype is 'acwrite'
    " oil.nvim
    if filetype is 'oil'
      " oil:///home/...
      " ^^^^^^
      let cwd = luaeval("require('oil').get_current_dir()")
      return cwd->simplify()->fnamemodify(':.') ?? '.'
    endif
  endif
  return empty(bufname) ? '[No Name]' : bufname
endfunction

function s:obsession() abort
  packadd vim-obsession
  let status = ObsessionStatus()->statusline#escape()
  let hl = status is '[$]' ? 'ObsessionActive' : 'ObsessionInactive'
  return $'%#{hl}#{status}%#StatusLine#'
endfunction

function s:fugitive() abort
  packadd vim-fugitive
  return FugitiveStatusline()->statusline#escape()
endfunction

function s:watch() abort
  " want to monitor something real-time? put it here
  return ''->statusline#escape()
endfunction
